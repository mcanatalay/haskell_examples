data Tree= Leaf String Int Int | Vertex String Int Int Tree Tree deriving Show

createLeaf name (brth,death)= Leaf name brth death
createVertex name (brth,death) l1 l2= Vertex name brth death l1 l2

halfVertex x= floor((fromIntegral(length x)-1)/2) --Finds the half points of vertex list
halfLeaf x= floor((fromIntegral(length x))/2) --Finds the half points of leaf list

familytree x d1 y d2= 
	if length y == 1 then --In base case it creats last vertexes and its leafs. All leafs are created here.
		createVertex  (head y) (head d2) (createLeaf (head x) (head d1)) (createLeaf (last x) (last d1))
	else --In this level it keeps apart to two x-leaf list- and y-vertex list- It tooks last element of y and gives half of x and y to left half of x and y to right
		let a1= take (halfLeaf x) x; t1= take (halfLeaf d1) d1; a2= drop (halfLeaf x) x; t2= drop (halfLeaf d1) d1;
			b1= take (halfVertex y) (init y); k1= take (halfVertex d2) (init d2); b2= drop (halfVertex y) (init y); k2= drop (halfVertex d2) (init d2) in
			createVertex (last y) (last d2) (familytree a1 t1 b1 k1) (familytree a2 t2 b2 k2)

--Sample family tree			
ftree= familytree ["Leonard","Darnell", "Crew","Crane"] [(1854, 1905), (1862,1916), (1856, 1907),(1862, 1916)] ["Latham", "Danny", "Kelby"] [(1823, 1908), (1825, 1900), (1800,1850)]

--This functions are peeks next nodes name	
nextNode (Vertex name brth death l1 l2)= name
nextNode (Leaf name brth death)= name

parent (Vertex name brth death l1 l2) str=
	if (nextNode l1) == str then --If next node is desired node returns this nodes name
		name
	else if (nextNode l2) == str then
		name
	else --We do this case because when one of nodes returning no parent other can be return name of parent
		let x = parent l1 str; y = parent l2 str in
			if x /= "no parent" then --If it is not no parent output returns it
				x
			else if y /= "no parent" then -- if it is not no parent output returns it
				y
			else
				"no parent"
		
parent (Leaf name brth death) str= "no parent" --If it reaches to leaf nodes it returns no parent

siblings (Vertex name brth death l1 l2) str=
	if (nextNode l1) == str then (nextNode l2) --if left node is given returns right node
	else if (nextNode l2) == str then (nextNode l1) --if right node is given returns left node
	else --Same for parent case
		let x = siblings l1 str; y = siblings l2 str in
			if x /= "no siblings" then
				x
			else if y /= "no siblings" then
				y
			else
				"no siblings"
				
siblings (Leaf name brth death) str= "no siblings" --If it reaches to leaf nodes it returns no siblings

sizeOfTree (Vertex name brth death l1 l2)=
	1+(sizeOfTree l1)+(sizeOfTree l2)

sizeOfTree (Leaf name brth death)=
	1
	
sumlifetime (Vertex name brth death l1 l2)=
	(death-brth)+(sumlifetime l1)+(sumlifetime l2)

sumlifetime (Leaf name brth death)=
	death-brth
	
avglifetime x=
	fromIntegral(sumlifetime x)/(sizeOfTree(x))
	

	


