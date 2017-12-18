import ProgFuncLista





testTripleFst = TestCase (assertEqual "Test of Fst element" 1 (tripleFst(Triple 1 2 3)))
testTripleSnd = TestCase (assertEqual "Test of Snd element" 2 (tripleSnd(Triple 1 2 3)))
testTripleThr = TestCase (assertEqual "Test of Thr element" 3 (tripleThr(Triple 1 2 3)))


testFirstTwo = TestCase (assertEqual "Test of firstTwo elements" (1,2) (firstTwo(Quadruple 1 2 3 4)))
testSecondTwo = TestCase (assertEqual "Test of secondTwo elements" (3,4) (secondTwo(Quadruple 1 2 3 4)))


testTuple1and1 = TestCase(assertEqual "Test Tuple 1 for 1 element" 1 (tuple1(Tuple1 1)))
testTuple1and2 = TestCase(assertEqual "Test Tuple 1 for 2 element" 1 (tuple1(Tuple2 1 2)))
testTuple1and3 = TestCase(assertEqual "Test Tuple 1 for 3 element" 1 (tuple1(Tuple3 1 2 3)))
testTuple1and4 = TestCase(assertEqual "Test Tuple 1 for 4 element" 1 (tuple1(Tuple4 1 2 3 4)))


testTuple2and2 = TestCase(assertEqual "Test Tuple 2 for 2 element" (Just 2) (tuple2(Tuple2 1 2)))
testTuple2and3 = TestCase(assertEqual "Test Tuple 2 for 3 element" (Just 2) (tuple2(Tuple3 1 2 3)))
testTuple2and4 = TestCase(assertEqual "Test Tuple 2 for 4 element" (Just 2) (tuple2(Tuple4 1 2 3 4)))


testTuple3and3 = TestCase(assertEqual "Test Tuple 3 for 3 element" (Just 3) (tuple3(Tuple3 1 2 3)))
testTuple3and4 = TestCase(assertEqual "Test Tuple 3 for 4 element" (Just 3) (tuple3(Tuple4 1 2 3 4)))

testTuple4and4 = TestCase(assertEqual "Test Tuple 4 for 4 element" (Just 4) (tuple4(Tuple4 1 2 3 4)))

testListLength = TestCase (assertEqual "Testa listLength para Nil" 0 (listLength Nil))


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


-- Testes BST - Igor Brasileiro --
-- fazer metodo fullfill?
arrayInsercao = [10, 18, 2, 13, 19, 8, -1]


fullFill lista = fullFillTree (tail lista) newTree
      where
            newTree = insert (head lista) NIL


fullFillTree [] tree = tree

fullFillTree lista tree = fullFillTree (tail lista) newTree
      where
            newTree = insert  (head lista) tree

tree1 = (Node 10 NIL NIL)
tree2 = (Node 10 NIL (Node 18 NIL NIL))
tree3 = (Node 10 (Node 2 NIL NIL) (Node 18 NIL NIL))
tree4 = (Node 10 (Node 2 NIL NIL) (Node 18 (Node 13 NIL NIL) NIL))
tree5 = (Node 10 (Node 2 NIL NIL) (Node 18 (Node 13 NIL NIL) (Node 19 NIL NIL)))
tree6 = (Node 10 (Node 2 NIL (Node 8 NIL NIL)) (Node 18 (Node 13 NIL NIL) (Node 19 NIL NIL)))
tree7 = (Node 10 (Node 2 (Node (-1) NIL NIL) (Node 8 NIL NIL)) (Node 18 (Node 13 NIL NIL) (Node 19 NIL NIL)))

treeSearchNeg1 = (Node (-1) NIL NIL)
treeSearch18 = (Node 18 (Node 13 NIL NIL) (Node 19 NIL NIL))
treeSearch13 = (Node 13 NIL NIL)

-- test insert
-- posso fazer somente os inserts e verificar a tree7?
testInsert10 =  TestCase (assertEqual "testInsert10" tree1 (insert 10 NIL))
testInsert18 = TestCase (assertEqual "testInsert18" tree2 (insert 18 tree1))
testInsert2 = TestCase (assertEqual "testInsert2" tree3 (insert 2 tree2))
testInsert13 = TestCase (assertEqual "testInsert13" tree4 (insert 13 tree3))
testInsert19 = TestCase (assertEqual "testInsert19" tree5 (insert 19 tree4))
testInsert8 = TestCase (assertEqual "testInsert8" tree6 (insert 8 tree5))
testInsertNeg1 = TestCase (assertEqual "testInsertNeg1" tree7 (insert (-1) tree6))

--test size
testSize1 = TestCase (assertEqual "testSize1" 7 (sizeBST tree7))
testSize2 = TestCase (assertEqual "testSize2" 6 (sizeBST tree6))
testSize3 = TestCase (assertEqual "testSize3" 1 (sizeBST tree1))
testSizeNIL = TestCase (assertEqual "testSizeNIL" 0 (sizeBST NIL))

-- test search
testSearchNeg1 = TestCase (assertEqual "testSearchNeg1" treeSearchNeg1 (search (-1) tree7))
testSearch18 = TestCase (assertEqual "testSearch18" treeSearch18 (search 18 tree7))
testSearch13 = TestCase (assertEqual "testSearch13" treeSearch13 (search 13 tree7))
testSearchRoot = TestCase (assertEqual "testSearchRoot" tree7 (search 10 tree7)) -- pesquisa root
testSearchNoExist = TestCase (assertEqual "testSearchNoExist" NIL (search 33 tree7))

-- test remove

-- remover 13
testRemove1 = TestCase (assertEqual "testRemove1" 6 sizeBST(remove 18 tree7))
testRemove1a = TestCase (assertEqual "testRemove1a" NIL (search 18 (remove 18 tree7))) -- remove da bst de no que nao existe retorna nil?
testRemove2 = TestCase (assertEqual "testRemove2" 5 sizeBST(remove 13 (remove 18 tree7)))
testRemove2a = TestCase (assertEqual "testRemove2a" NIL (search 13 (remove 13 (remove 18 tree7))))
testRemove2b = TestCase (assertEqual "testRemove2b" (Node 19 NIL NIL) (search 19 (remove 13 (remove 18 tree7))))

testRemove3 = TestCase (assertEqual "testRemove3" 6 sizeBST(remove 8 tree7))
testRemove3a = TestCase (assertEqual "testRemove3a" NIL (search 6 tree7))
testRemove3a = TestCase (assertEqual "testRemoveNIL" NIL (remove 10 NIL))


-- test maximum
testmaximumTree1 = TestCase (assertEqual "testmaximumTree1" 10  (maximum (tree1))
testmaximumTree2 = TestCase (assertEqual "testmaximumTree2" 18  (maximum (tree2))
testmaximumTree3 = TestCase (assertEqual "testmaximumTree3" 18  (maximum (tree3))
testmaximumTree4 = TestCase (assertEqual "testmaximumTree4" 18  (maximum (tree3))
testmaximumTree5 = TestCase (assertEqual "testmaximumTree5" 19  (maximum (tree3))
testmaximumTree6 = TestCase (assertEqual "testmaximumTree6" 19  (maximum (tree3))
testmaximumTree7 = TestCase (assertEqual "testmaximumTree7" 19  (maximum (tree3))

-- test minimu
testminimuTree1 = TestCase (assertEqual "testminimuTree1" 10  (minimu (tree1))
testminimuTree2 = TestCase (assertEqual "testminimuTree2" 10  (minimu (tree2))
testminimuTree3 = TestCase (assertEqual "testminimuTree3" 2  (minimu (tree3))
testminimuTree4 = TestCase (assertEqual "testminimuTree4" 2  (minimu (tree3))
testminimuTree5 = TestCase (assertEqual "testminimuTree5" 2  (minimu (tree3))
testminimuTree6 = TestCase (assertEqual "testminimuTree6" 2  (minimu (tree3))
testminimuTree7 = TestCase (assertEqual "testminimuTree7" -1  (minimu (tree3))  

-- test order
testOrderTree1 = TestCase (assertEqual "testOrderTree1" [10]  (order (tree1))
testOrderTree2 = TestCase (assertEqual "testOrderTree2" [10, 18]  (order (tree2))
testOrderTree3 = TestCase (assertEqual "testOrderTree3" [2, 10, 18]  (order (tree3))
testOrderTree4 = TestCase (assertEqual "testOrderTree4" [2, 10, 13, 18]  (order (tree4))
testOrderTree5 = TestCase (assertEqual "testOrderTree6" [2, 10, 13, 18, 19]  (order (tree5))
testOrderTree6 = TestCase (assertEqual "testOrderTree6" [2, 8, 10, 13, 18, 19]  (order (tree6))
testOrderTree7 = TestCase (assertEqual "testOrderTree7" [-1, 2, 8, 10, 13, 18, 19]  (order (tree7))
testOrderNIL = TestCase (assertEqual "testOrderNIL" []  (order (NIL))

-- test preOrder 
testPreOrderTree1 = TestCase (assertEqual "testPreOrderTree1" [10]  (preOrder (tree1))
testPreOrderTree2 = TestCase (assertEqual "testPreOrderTree2" [10, 18]  (preOrder (tree2))
testPreOrderTree3 = TestCase (assertEqual "testPreOrderTree3" [10, 2, 18]  (preOrder (tree3))
testPreOrderTree4 = TestCase (assertEqual "testPreOrderTree4" [10, 2, 18, 13]  (preOrder (tree4))
testPreOrderTree5 = TestCase (assertEqual "testPreOrderTree5" [10, 2, 18, 13, 19]  (preOrder (tree5))
testPreOrderTree6 = TestCase (assertEqual "testPreOrderTree6" [10, 2, 8, 18, 13, 19]  (preOrder (tree6))
testPreOrderTree7 = TestCase (assertEqual "testPreOrderTree7" [10, 2, -1, 8, 18, 13, 19]  (preOrder (tree7))
testPreOrderNIL = TestCase (assertEqual "testPreOrderNIL" []  (preOrder (NIL))

-- test postOrder
testPostOrderTree1 = TestCase (assertEqual "testPostOrderTree1" [10]  (postOrder (tree1))
testPostOrderTree2 = TestCase (assertEqual "testPostOrderTree2" [18, 10]  (postOrder (tree2))
testPostOrderTree3 = TestCase (assertEqual "testPostOrderTree3" [2, 18, 10]  (postOrder (tree3))
testPostOrderTree4 = TestCase (assertEqual "testPostOrderTree4" [2, 13, 18, 10]  (postOrder (tree4))
testPostOrderTree5 = TestCase (assertEqual "testPostOrderTree5" [2, 13, 19, 18, 10]  (postOrder (tree5))
testPostOrderTree6 = TestCase (assertEqual "testPostOrderTree6" [8, 2, 13, 19, 18, 10]  (postOrder (tree6))
testPostOrderTree7 = TestCase (assertEqual "testPostOrderTree7" [-1, 8, 2, 13, 19, 18, 10]  (postOrder (tree7))
testPostOrderNIL = TestCase (assertEqual "testPostOrderNIL" []  (postOrder (NIL))


-- Termino Testes BST - 



tests = TestList [testsize1,testsize2,testsize3,testisbst1,testisbst2,
                  testisbst3,testinsert1,testinsert2,testinsert3,testsearch1,
                  testsearch2,testsearch3,testpredecessor1,testpredecessor2,
                  testsuccessor1,testsuccessor2,testremove1,testremove2,
                  testremove3,testremove4,testpreorder,testorder,testpostorder,
                  testInsert10, testInsert18, testInsert2, testInsert13, testInsert19,
                  testInsert8, testInsertNeg1, testSize1, testSize2, testSize3, testSizeNIL,
                  testSearchNeg1, testSearch18, testSearch13, testSearchRoot, testSearchNoExist
                  testRemove1, testRemove1a, testRemove2, testRemove2a, testRemove2b, testRemove3,
                  testRemove3a, testRemove3atestmaximumTree1, testmaximumTree2, testmaximumTree3,
                  testmaximumTree4, testmaximumTree5, testmaximumTree6, testmaximumTree7, 
                  testminimuTree1, testminimuTree2, testminimuTree3, testminimuTree4, testminimuTree5, 
                  testminimuTree6, testminimuTree7, testOrderTree1, testOrderTree2, testOrderTree3, testOrderTree4,
                  testOrderTree5, testOrderTree6, testOrderTree7, testOrderNIL, testPreOrderTree1, testPreOrderTree2, testPreOrderTree3,
                  testPreOrderTree4, testPreOrderTree5, testPreOrderTree6, testPreOrderTree7, testPreOrderNIL, testPostOrderTree1, 
                  testPostOrderTree2, testPostOrderTree3, testPostOrderTree4, testPostOrderTree5, testPostOrderTree6,
                  testPostOrderTree7, testPostOrderNIL]

-- Por favor atualizar esta lista
{- 
  sizeBST - Igor - OK
  isBST - Igor - OK
  insert - Igor - OK
  search - Igor - OK
  maximu - Wagner - ok
  minimu - Wagner - ok
  predecessor 
  sucessor
  remove - Igor - OK
  preOrder - Wagner - OK
  postOrder - Wagner - OK
  order - Wagner - Ok
-}

