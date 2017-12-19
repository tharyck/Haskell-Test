module TipoDeDados (
	Triple(Triple),
	Quadruple(Quadruple),
	Tuple(Tuple1, Tuple2, Tuple3, Tuple4),
	List(Nil, Cons),
	BinaryTree(..),
	tripleFst,
	tripleSnd,
	tripleThr,
	firstTwo,
	secondTwo,
	tuple1,
	tuple2,
	tuple3,
	tuple4,
	listLength,
	listHead,
	listTail,
	listFoldr,
	listFoldl,
	sizeBST,
	isBST,
	insert,
	search,
	maximum,
	minimum,
	successor,
	predecessor,
	remove,
	preOrder,
	order,
	postOrder)
	where
	import Prelude hiding (maximum, minimum)
-- Fazer REMOVE DA BST, predecessor do menor ou sucessor do maior está dando problema

--Escreva a declaracao para o tipo Triple, contendo tres elementos, todos de tipos diferentes.
--Escreva funcoes tripleFst, tripleSnd, tripleThr para extrair respectivamente o primeiro, segundo e terceiro
-- elementos de uma triple.

	data Triple a b c = Triple a b c deriving (Eq,Show)

	tripleFst (Triple a b c) = a
	tripleSnd (Triple a b c) = b
	tripleThr (Triple a b c) = c

	--Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
	--Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente
	data Quadruple a b = Quadruple a a b b deriving(Eq, Show)

	firstTwo (Quadruple a b c d) = (a,b)  -- ter cuidado com isso
	secondTwo (Quadruple a b c d) = (c,d)

	--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
	--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
	data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d deriving(Eq, Show)

	tuple1 (Tuple1 a) = a
	tuple1 (Tuple2 a b) = a
	tuple1 (Tuple3 a b c) = a 
	tuple1 (Tuple4 a b c d) = a

	tuple2 (Tuple2 a b) = Just b
	tuple2 (Tuple3 a b c) = Just b 
	tuple2 (Tuple4 a b c d) = Just b
	tuple2 _ = Nothing


	tuple3 (Tuple3 a b c) = Just c
	tuple3 (Tuple4 a b c d) = Just c
	tuple3 _ = Nothing

	tuple4 (Tuple4 a b c d) = Just d
	tuple4 _ = Nothing

	data List a = Nil | Cons a (List a) deriving (Eq,Show)

	listLength Nil = 0
	listLength (Cons x xs) = 1 + listLength xs

	listHead Nil = error "Empty list"
	listHead (Cons x xs) = x

	listTail Nil = Nil
	listTail (Cons x xs) = xs

	listFoldr f v Nil = v
	listFoldr f v (Cons x xs) = f x (listFoldr f v xs)


	listFoldl f v Nil = v
	listFoldl f v (Cons x xs) = listFoldl f (f v x) xs 

	--Escreva as funcoes sobre a estrutura de dados binary tree
	data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a)
	 deriving (Eq,Show)

	sizeBST NIL = 0
	sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

	--verifica se uma BT é uma BST
	-- * false if the max of the left is > than us */
	--   /* false if the min of the right is <= than us */
	-- /* false if, recursively, the left or right is not a BST */
	-- else true
	isBST NIL = True
	isBST (Node a NIL NIL) = True
	isBST (Node a left NIL) = if (maximum (left)) > a then False else isBST left
	isBST (Node a NIL right) = if (minimum(right)) < a then False else isBST right
	isBST (Node a left right) = 
		if (maximum (left)) > a 
			then False 
			else if (minimum (right)) < a
				then False
				else if not (isBST (left)) || not (isBST (right))
					then False
					else True


	--insere uma nova chave na BST retornando a BST modificada
	insert e NIL = Node e NIL NIL
	insert e (Node a left right) = 
		if e <= a 
			then Node a (insert e left) right 
			else Node a left (insert e right) 

	--retorna o Node da BST contendo o dado procurado ou entao NIL
	search e NIL= NIL
	search e (Node a left right) = if a == e then (Node a left right) else if e <= a then search e left else search e right

	--retorna o elmento maximo da BST

	maximum (Node a _ NIL) = a
	maximum (Node a _ right) = maximum right

	--retorna o elemento minimo da BST

	minimum (Node a NIL _) = a
	minimum (Node a left _) = minimum left

	--retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
	predecessor e (Node root left right)
		| root == e && left /= NIL = maximum left
		| e < root = predecessor e left
		| e > root = predecessor e right

	--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
	successor e (Node root left right)
		| root == e && right /= NIL = minimum right
		| e < root = successor e left
		| e > root = successor e right

	--remove um elemento da BST
	remove b (Node a NIL NIL) = 
		if a == b
			then NIL
			else (Node a NIL NIL)


	--retorna uma lista com os dados da BST nos diversos tipos de caminhamento
	preOrder NIL = []
	preOrder (Node a NIL NIL) = [a]
	preOrder (Node a left right) = [a] ++ (preOrder left) ++ (preOrder right)

	order NIL = []
	order (Node a NIL NIL) = [a]
	order (Node a left right) = (order left) ++ [a] ++ (order right)

	postOrder NIL = []
	postOrder (Node a NIL NIL) = [a]
	postOrder (Node a left right) = (postOrder left) ++ (postOrder right) ++ [a]