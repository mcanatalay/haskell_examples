{-|
data Tree= Node String (Integer,Integer) [Tree] deriving Show   --Only difference in this type it uses list of trees

createNode name (brth,death) x= Node name (brth,death) x

countDrop 0 []= 0
countDrop 0 y= 0
countDrop x y= 
	1 + countDrop (x-1+(head y)) (tail y)
	
count (y:ys)= countDrop y ys

familytree [][][]= []
familytree x y d=
	let dvalue = count y in
		[createNode (head x) (head d) (familytree (take dvalue (tail x)) (take dvalue (tail y)) (take dvalue (tail d)))] ++ (familytree (drop dvalue (tail x)) (drop dvalue (tail y)) (drop dvalue (tail d)))
-}

data Tree=  EmptyTree | Node String (Integer,Integer) Tree Tree deriving Show

createNode name (brth,death) child next= Node name (brth,death) child next --Child indicates children nodes while next indicates sibling nodes

countDrop 0 []= 0 
countDrop 0 y= 0
countDrop x y= --This function counts how many grand-children node given node has
	1 + countDrop (x-1+(head y)) (tail y)
	
count (y:ys)= countDrop y ys

familytree [][][]= EmptyTree
familytree x y d=
	let dvalue = count y in
		createNode (head x) (head d) (familytree (take dvalue (tail x)) (take dvalue (tail y)) (take dvalue (tail d))) (familytree (drop dvalue (tail x)) (drop dvalue (tail y)) (drop dvalue (tail d)))

--Sample tree to use
ftree= familytree ["Nikolaus1", "Jacob I", "Nikolaus2", "Nikolaus I", "Johann I", "Nikolaus II", "Daniel","Johann II", "Johann III", "Jacob II"] [3, 0, 1, 0, 3, 0, 0, 2, 0, 0] [(1623, 1708), (1654, 1705), (1662,1716), (1687,1759), (1667,1748), (1695, 1726), (1700, 1782), (1710, 1790), (1746, 1807), (1759, 1789)]
