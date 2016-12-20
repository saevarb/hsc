{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Util where

import Control.Monad.State
import Control.Monad.Writer 
import System.Random
import Text.Printf
import Data.Text (Text)
import qualified Data.Text as T

import Shelly hiding (ls)
import Control.Monad.Random

import Types

default (T.Text, Double)

type GraphM = RandT StdGen (State Graph)

data Node
    = Node
    { repr  :: String
    , ident :: Int
    }
    deriving (Show, Eq, Ord)

data Graph
    = Graph
    { nodes :: [Node]
    , edges :: [(Node, Node)]
    }
    deriving (Show)


createNode :: (MonadState Graph m, MonadRandom m) => String -> m Node
createNode repr = do
    rid <- getRandomR (0, maxBound)
    let node = Node repr rid
    modify $ \s -> s { nodes = node : nodes s }
    return node

link n1 n2 = do
    let l = (n1, n2)
    g <- gets edges
    modify $ \s -> s { edges = l : edges s }

renderGraph :: Graph -> String
renderGraph g =
    unlines $ begin ++ nodes' ++ edges' ++ end
  where
    begin = ["digraph {"]
    nodes' = map renderNode (nodes g)
    edges' = map renderEdge (edges g)
    end = ["}"]

renderNode :: Node -> String
renderNode (Node r i) =
    printf "%d [label=%s];" i (show r)

renderEdge :: (Node, Node) -> String
renderEdge (n1, n2) =
    printf "%d -> %d;" (ident n1) (ident n2)

visualizeAST :: Body -> IO [Char]
visualizeAST ast = do
    filename <- replicateM 8 (randomRIO ('a', 'z'))
    gen <- getStdGen
    let s = Graph [] []
        graph = execState (runRandT (bodyNodes ast) gen) s
    writeFile filename (renderGraph graph)
    return filename

visualizeAndShow :: Body -> IO ()
visualizeAndShow t = do
    filename <- visualizeAST t
    shelly $ do
        let asText = toTextArg filename
            png = toTextArg $ filename ++ ".png"
        void $ cmd "dot" "-Tpng" asText "-o" png
        void $ cmd "sxiv" png
        void $ cmd "rm" asText png


defRender :: (Show a) => a -> GraphM Node
defRender n = createNode (show n)

stmtNodes :: Stmt -> GraphM Node
stmtNodes (RetStmt e) = do
    n <- createNode "return"
    r <- expNodes e
    link n r 
    return n
stmtNodes x = defRender x

declNodes = defRender

expNodes :: Exp -> GraphM Node
expNodes (ConstExp e) = createNode (show e)
expNodes (BinExp e1 op e2) = do
    n <- createNode (show op)
    mapM_ (expNodes >=> link n) [e1, e2]
    return n
expNodes x = defRender x

bodyNodes (Body ds ss) = do
    body <- createNode "(body)"
    stmts <- mapM stmtNodes ss
    decls <- mapM declNodes ds
    mapM_ (link body) stmts
    mapM_ (link body) decls
