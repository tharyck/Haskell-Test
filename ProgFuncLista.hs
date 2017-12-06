import Test.HUnit

data Maybe x = Just x | Nothing
--Escreva a declaracao para o tipo Triple, contendo tres elementos, todos de tipos diferentes.
--Escreva funcoes tripleFst, tripleSnd, tripleThr para extrair respectivamente o primeiro, segundo e terceiro
-- elementos de uma triple.
data Triple a b c = Triple a b c deriving (Eq,Show)

tripleFst (Triple a b c) = a
tripleSnd (Triple a b c) = b
tripleThr (Triple a b c) = c

testTripleFst = TestCase (assertEqual "Test of FST element" 1 (tripleFst(Triple 1 2 3)))
testTripleSnd = TestCase (assertEqual "Test of Snd element" 2 (tripleFst(Triple 1 2 3)))
testTripleThr = TestCase (assertEqual "Test of Thr element" 3 (tripleFst(Triple 1 2 3)))
-- Falta assert Error com teste null



--Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
--Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente
data Quadruple a b = Quadruple a a b b

firstTwo (Quadruple a b c d) = (a,b)
secondTwo (Quadruple a b c d) = (c,d)

testFirstTwo = TestCase (assertEqual "Test of firstTwo elements" (1,2) (firstTwo(Quadruple 1 2 3 4)))
testSecondTwo = TestCase (assertEqual "Test of secondTwo elements" (3,4) (secondTwo(Quadruple 1 2 3 4)))
-- Falta assert Error com teste null

--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
data Tuple a b c d = TupleA a | TupleB a b | TupleC a b c | TupleD a b c d  deriving (Eq,Show)

tuple1 = undefined
tuple2 = undefined
tuple3 = undefined
tuple4 = undefined

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
isBST NIL = True
isBST (Node a NIL NIL) = True
isBST (Node a (Node b left right) NIL) = b <= a && isBST (Node b left right)
isBST (Node a NIL (Node b left right)) = b >= a && isBST (Node b left right)
isBST (Node a (Node b left right) (Node c lleft rright)) = b <= a && a <= c && isBST (Node b left right) && isBST (Node c lleft rright)

--insere uma nova chave na BST retornando a BST modificada
insert x NIL = (Node x) NIL NIL
insert x (Node a (left) (right)) = if x < a
  then Node a (insert x left) (right)
  else Node a (left) (insert x right)

--retorna o Node da BST contendo o dado procurado ou entao NIL
search x NIL = NIL
search x (Node a (left) (right)) = if x == a
  then (Node a (left)(right))
  else (if x < a
    then search x (left)
    else search x (right))

--retorna no com elmento maximo da BST
--maximum NIL = NIL
--maximum (Node a left NIL) = Node a left NIL
--maximum (Node a left right)= Main.maximum right

--retorna no com elemento minimo da BST
--minimum NIL = NIL
--minimum (Node a NIL right) = (Node a NIL right)
--minimum (Node a left right)= Main.minimum left

--retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
predecessor x (Node a left right) = if search x (Node a left right) /= NIL
  then (order (Node a left right) !! (position x (Node a left right) - 1))
  else x
--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST

successor x (Node a left right) = if search x (Node a left right) /= NIL
  then (order (Node a left right) !! (position x (Node a left right) + 1))
  else x

--Remove um dado elemento
remove x NIL = NIL
remove x (Node a left right) = if x == a
  then remove_action x (Node a left right)
  else (if x < a
    then Node a (remove x (left)) right
    else Node a left (remove x (right)))
--remove ume lemento da BST
remove_action x NIL = NIL
--elemento eh folha
remove_action x (Node b NIL NIL) = NIL
--elemento tem um filho
remove_action x (Node b left NIL) = left

remove_action x (Node b NIL right) = right
--elemento tem dois filhos
remove_action x (Node b left right) = Node (successor b (Node b NIL right)) left (remove (successor b (Node b NIL right)) right)

--retorna uma lista de nos com os dados da BST nos diversos tipos de caminhamento
preOrder (Node a NIL NIL) = [getDate (Node a NIL NIL)]
preOrder (Node a left NIL) = [getDate (Node a NIL NIL)] ++ preOrder left
preOrder (Node a NIL right) = [getDate (Node a NIL NIL)] ++ preOrder right
preOrder (Node a left right) = ([getDate (Node a NIL NIL)] ++ preOrder left) ++ preOrder right

order (Node a NIL NIL) = [getDate (Node a NIL NIL)]
order (Node a left NIL) = order left ++ [getDate (Node a NIL NIL)]
order (Node a NIL right) = [getDate (Node a NIL NIL)] ++ order right
order (Node a left right) = (order left ++ [getDate (Node a NIL NIL)]) ++ order right

postOrder (Node a NIL NIL) = [getDate (Node a NIL NIL)]
postOrder (Node a left NIL) = postOrder left ++ [getDate (Node a NIL NIL)]
postOrder (Node a NIL right) = postOrder right ++ [getDate (Node a NIL NIL)]
postOrder (Node a left right) = (postOrder left ++ postOrder right) ++ [getDate (Node a NIL NIL)]

getDate (Node a left right) = a

--retorna No com dado de acordo com a sua posição
position x NIL = -1
position x (Node a left right) = if search x (Node a left right) == NIL
  then -1
  else position_search x (order (Node a left right)) 0

position_search x [] p = -1
position_search x (a : xs) p = if x == a
  then p
  else position_search x (xs) (p + 1)

testsize1 = TestCase (assertEqual "size of empty tree" 0 (sizeBST NIL))
testsize2 = TestCase (assertEqual "size of very unbalanced tree" 5 (sizeBST (Node 3 NIL (Node 4 NIL (Node 5 NIL (Node 9 (Node 7 NIL NIL) NIL))) )))
testsize3 = TestCase (assertEqual "size of balanced tree" 3 (sizeBST (Node 3 (Node (-1) NIL NIL) (Node 4 NIL NIL))))

testisbst1 = TestCase (assertEqual "correct BST tree" True (isBST (Node 10 (Node 0 NIL (Node 5 NIL NIL)) (Node 20 (Node 15 NIL NIL) (Node 40 NIL NIL)))))
testisbst2 = TestCase (assertEqual "node with 7 in wrong position" False (isBST (Node 10 (Node 5 (Node 1 NIL NIL) (Node 6 NIL NIL)) (Node 20 (Node 7 NIL NIL) (Node 99 NIL NIL)))))
testisbst3 = TestCase (assertEqual "node with 17 in wrong position" False (isBST (Node 10 (Node 5 NIL (Node 17 NIL NIL)) (Node 20 NIL NIL))))

testinsert1 = TestCase (assertEqual "insert in left position" (Node 10 (Node 5 NIL NIL) (Node 20 (Node 17 NIL NIL) NIL)) (insert 17 (Node 10 (Node 5 NIL NIL) (Node 20 NIL NIL)) ))
testinsert2 = TestCase (assertEqual "insert in right position" (Node 10 (Node 5 NIL NIL) (Node 20 NIL (Node 22 NIL NIL))) (insert 22 (Node 10 (Node 5 NIL NIL) (Node 20 NIL NIL)) ))
testinsert3 = TestCase (assertEqual "insert in an empty tree" (Node 40 NIL NIL) (insert 40  NIL))

testsearch1 = TestCase (assertEqual "search tree's node " (Node 90 NIL (Node 100 NIL NIL)) (search 90 (Node 10 (Node 2 NIL NIL) (Node 90 NIL (Node 100 NIL NIL)))))
testsearch2 = TestCase (assertEqual "search tree's root" (Node 10 (Node 2 NIL NIL) (Node 90 NIL (Node 100 NIL NIL))) (search 10 (Node 10 (Node 2 NIL NIL) (Node 90 NIL (Node 100 NIL NIL)))))
testsearch3 = TestCase (assertEqual "search for node that hasn't in tree" NIL (search 110 (Node 10 (Node 2 NIL NIL) (Node 90 NIL (Node 100 NIL NIL)))))

--testmaximum1 = TestCase (assertEqual "testmaximum1" 20  (maximum (Node 10 (Node 5 NIL NIL) (Node 20 NIL NIL)) ))
--testmaximum2 = TestCase (assertEqual "testmaximum2" 9  (maximum (Node 5 NIL (Node 6 NIL (Node 7 NIL (Node 8 NIL (Node 9 NIL NIL)))) ) ))

--testminimum1 = TestCase (assertEqual "testminimum1" 5  (minimum (Node 10 (Node 5 NIL NIL) (Node 20 NIL NIL)) ))
--testminimum2 = TestCase (assertEqual "testminimum2" 1  (minimum (Node 5 (Node 4 (Node 3 (Node 2 (Node 1 NIL NIL) NIL) NIL) NIL) NIL) ))

testpredecessor1 = TestCase (assertEqual "predecessor in some lower position" 1  (predecessor 2 (Node 5 (Node 4 (Node 3 (Node 2 (Node 1 NIL NIL) NIL) NIL) NIL) NIL) ))
testpredecessor2 = TestCase (assertEqual "predecessor in some higher position" 90  (predecessor 100 (Node 10 (Node 2 NIL NIL) (Node 90 NIL (Node 100 NIL NIL)))))

testsuccessor1 = TestCase (assertEqual "successor in some lower position" 100  (successor 90 (Node 10 (Node 2 NIL NIL) (Node 90 NIL (Node 100 NIL NIL)))))
testsuccessor2 = TestCase (assertEqual "successor in some higher position" 3  (successor 2 (Node 5 (Node 4 (Node 3 (Node 2 (Node 1 NIL NIL) NIL) NIL) NIL) NIL) ))

testremove1 = TestCase (assertEqual "remove node without children" (Node 10 (Node 2 NIL NIL) NIL) (remove 20 (Node 10 (Node 2 NIL NIL) (Node 20 NIL NIL))))
testremove2 = TestCase (assertEqual "remove node that hasn't in tree" (Node 10 (Node 2 NIL NIL) (Node 20 NIL NIL)) (remove 90 (Node 10 (Node 2 NIL NIL) (Node 20 NIL NIL))))
testremove3 = TestCase (assertEqual "remove node with single children" (Node 10 (Node 2 NIL NIL) (Node 100 NIL NIL)) (remove 90 (Node 10 (Node 2 NIL NIL) (Node 90 NIL (Node 100 NIL NIL)))))
testremove4 = TestCase (assertEqual "remove node with dual childrens (get successor's way)" (Node 90 (Node 2 NIL NIL) (Node 100 NIL NIL)) (remove 10 (Node 10 (Node 2 NIL NIL) (Node 90 NIL (Node 100 NIL NIL)))))

testpreorder = TestCase (assertEqual "testpreorder" [3,2,1,4] (preOrder (Node 3 (Node 2 (Node 1 NIL NIL) NIL) (Node 4 NIL NIL))))
testorder = TestCase (assertEqual "testorder" [1,2,3,4] (order (Node 3 (Node 2 (Node 1 NIL NIL) NIL) (Node 4 NIL NIL))))
testpostorder = TestCase (assertEqual "testpostorder" [1,2,4,3] (postOrder (Node 3 (Node 2 (Node 1 NIL NIL) NIL) (Node 4 NIL NIL))))

tests = TestList [testsize1,testsize2,testsize3,testisbst1,testisbst2,testisbst3,testinsert1,testinsert2,testinsert3,testsearch1,testsearch2,testsearch3,testpredecessor1,testpredecessor2,testsuccessor1,testsuccessor2,testremove1,testremove2,testremove3,testremove4,testpreorder,testorder,testpostorder]
