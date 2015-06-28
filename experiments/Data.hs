import System.CPUTime
import Data.Time.Calendar
import Data.List

data DataTypes = Boolean Bool | Double Double | Date Int | String String | Int Int  deriving (Eq, Show)

type Record = [DataTypes]
type Table = [Record]

gs :: DataTypes -> String
gs (String s) = s

-- sort by 2nd column:
stupidSort = sortBy (\x y -> compare (gs $ x!!1)  (gs $ y!!1) )

-- then we add groupBy over the sorted list and then can do different folds - that should cover pivot functionality

testTable = [ 
	[Double 2435, String "NA", Date 1, Boolean True, Int 1241],
	[Double 52435, String "EM", Date 3, Boolean True],
	[Double 142435, String "EMEA", Date 1, Boolean True, Int 131],
	[Double 22535, String "NA", Date 2, Boolean True, Int 141],
	[Double 9435, String "EMEA", Date 2]
	]


-- Naive Graph based on JValue. Eventually, we may want to add support for Haskell datatypes via type classes ('synchronizable or something')

data KValue = KString String
            | KNumber Double
            | KBool Bool
            | KNull
            | KArray [KValue]
              deriving (Eq, Ord, Show)

type KVP = [(String, KValue)]

getString :: KValue -> Maybe String
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


-- stub for typical Graph functions: to be expanded and used for testing with different backends

-- stupid placeholder for now
type Graph = Int
type Node = Int
type Edge = Int
type GValue = KVP

-- checks if 2 nodes are adjacent
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

-- manipulating Node and Edge values:
getNodeValue :: Graph -> Node -> GValue
getNodeValue g x = []

getEdgeValue :: Graph -> Node -> Node -> GValue
getEdgeValue g x y = []

setNodeValue :: Graph -> Node -> GValue -> Bool
setNodeValue g x v = True

setEdgeValue :: Graph -> Node -> Node -> GValue -> Bool
setEdgeValue g x y v = True









