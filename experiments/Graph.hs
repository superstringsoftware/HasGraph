{-
Some data-aware Graph abstractions. 
After some considerations, the idea is to use some form of simple "reference"-like Graph structure
(only node ids and edge ids in some form - which abstract away pointers at the db layer or array indices etc).
We will store data in Hashtables, since apparently their performance has been fixed.
We will explore 2 approaches:
1) Use 1 Hashtable for ALL Nodes and another for ALL Edges
2) Use 1 Hashtable for EACH label type of Node and Edge
Starting with 2.
-}

module HasGraph.Graph (
	KValue,
	KVP,
	getString,
	getInt,
	getDouble,
	getBool,
	getArray,
	isNull,
	ObjectID,
	FastString,
	NumIndex,
	Edge,
	Node,
	Graph
)
where

import Data.List
import Control.Monad.ST

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T

import qualified Data.HashTable.ST.Basic as HT
import qualified Data.HashTable.Class as H


-- Naive Graph based on JValue. Eventually, we may want to add support for Haskell datatypes via type classes ('synchronizable or something')

data KValue = KString T.Text
            | KNumber Double
            | KBool Bool
            | KNull
            | KArray [KValue]
              deriving (Eq, Ord, Show)

type KVP = [(FastString, KValue)]

getString :: KValue -> Maybe T.Text
getString (KString s) = Just s
getString _           = Nothing

getInt (KNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble (KNumber n) = Just n
getDouble _           = Nothing

getBool (KBool b) = Just b
getBool _         = Nothing

getArray (KArray a) = Just a
getArray _          = Nothing

isNull v            = v == KNull

-- Definitions for the graph structure
type ObjectID = C8.ByteString
type FastString = C8.ByteString
type NumIndex = Int

data Edge = Edge {
		edgeId :: ObjectID
	,	edgeLabel :: FastString
	,	fromNode :: ObjectID
	,	toNode :: ObjectID
	,	edgeData :: KVP 
} deriving (Show)

data Node = Node { 
		nodeId :: ObjectID
	,	nodeLabel :: FastString
	, 	outEdges :: [(FastString, [Edge])] -- list of Edges for every Node Label (needed for search optimizations)
	,	nodeData :: KVP
} deriving (Show)


-- defining mutable hashtable that will hold our Nodes (Edges are contained within Nodes)
type Graph s = HT.HashTable s ObjectID Node

-- generating empty graph and now filling it with some stupid placeholders for testing
generateGraph :: ST s (Graph s)
generateGraph = do
  ht <- HT.new
  HT.insert ht (C8.pack "key0") Node {nodeId = C8.pack "key0", nodeLabel = C8.pack "Person", nodeData = [], outEdges = []}
  HT.insert ht (C8.pack "key1") Node {nodeId = C8.pack "key1", nodeLabel = C8.pack "Person", nodeData = [], outEdges = []}
  HT.insert ht (C8.pack "key2") Node {nodeId = C8.pack "key2", nodeLabel = C8.pack "Person", nodeData = [], outEdges = []}
  return ht

-- finds a node by ID in a given graph
findNodeById :: ObjectID -> Graph s -> ST s (Maybe Node)
findNodeById k g = do
  val <- HT.lookup g k
  return val

-- insert a new node. TODO: ObjectID generation.
insertNode :: Node -> Graph s -> ST s (Graph s)
insertNode n g = do
  HT.insert g (nodeId n) n 
  return g

testNode = Node {nodeId = C8.pack "key5", nodeLabel = C8.pack "Mail", nodeData = [], outEdges = []}

-- test insertion
-- testInsertion = runST (insertNode Node {nodeId = C8.pack "key5", nodeLabel = C8.pack "Mail", nodeData = [], outEdges = []}  >>= findNodeById "key1")
-- unwrap1 :: ST s (Graph s) -> Graph s
-- unwrap1 (ST s a) = a 


-- for now, this describes a graph based on the adjacency table, while nodes are stored in an Array. May want to add adjacency matrix / hashtable based on (x,y) node for quick checks for neigbors

-- checks if 2 nodes are adjacent
{-
adjacent :: Graph -> Node -> Node -> Bool
adjacent g x y = True

-- gets all neighboring nodes
neighbors :: Graph -> Node -> [Node]
neighbors g x = []

-- add and delete an edge between 2 nodes
add :: Graph -> Node -> Node -> Bool
add g x y = True

delete :: Graph -> Node -> Node -> Bool
delete g x y = True
-}

-- Small test graph

{-
tg = [
	Node {nid = GID {oid = C8.pack "0", numId = 0, label = C8.pack "Person"} 
		, nodeData = [(C8.pack "name", KString $ T.pack "Anton"), (C8.pack "Age", KNumber 37)]
		, outEdges = []
		, inEdges = []
	}, 
	Node {nid = GID {oid = C8.pack "1", numId = 1, label = C8.pack "Person"} 
		, nodeData = [(C8.pack "name", KString $ T.pack "Maria"), (C8.pack "Age", KNumber 35)]
		, outEdges = []
		, inEdges = []
	}, 
	Node {nid = GID {oid = C8.pack "2", numId = 2, label = C8.pack "Person"} 
		, nodeData = [(C8.pack "name", KString $ T.pack "John"), (C8.pack "Age", KNumber 29)]
		, outEdges = []
		, inEdges = []
	}, 
	Node {nid = GID {oid = C8.pack "3", numId = 3, label = C8.pack "Person"} 
		, nodeData = [(C8.pack "name", KString $ T.pack "Maria"), (C8.pack "Age", KNumber 31)]
		, outEdges = []
		, inEdges = []
	}, 
	Node {nid = GID {oid = C8.pack "4", numId = 4, label = C8.pack "Person"} 
		, nodeData = [(C8.pack "name", KString $ T.pack "James"), (C8.pack "Age", KNumber 22)]
		, outEdges = []
		, inEdges = []
	}, 
	Node {nid = GID {oid = C8.pack "5", numId = 5, label = C8.pack "Person"} 
		, nodeData = [(C8.pack "name", KString $ T.pack "Gloria"), (C8.pack "Age", KNumber 40)]
		, outEdges = []
		, inEdges = []
	}
	]
	-}
