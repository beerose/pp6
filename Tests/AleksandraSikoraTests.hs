{-# LANGUAGE Safe #-}

module AleksandraSikoraTests(tests) where

import DataTypes


tests :: [Test]
tests =
  [ 
      
    Test "inc"      (SrcString "input x in x + 1") (Eval [42] (Value 43))
    , 
    Test "undefVar" (SrcString "x")                TypeError
    , 
    Test "wrongTypes" (SrcString "2 + true") TypeError
    , 
    Test "testEnum" (SrcString "5 = true") TypeError
    , 
    Test "testUnaryNot" (SrcString "not 5") TypeError
    , 
    Test "testUnaryNeg" (SrcString "- true") TypeError
    , 
    Test "testBinaryOperatorAdd" (SrcString "true + false") TypeError
    , 
    Test "testBinaryOperatorSub" (SrcString "true - 2") TypeError
    , 
    Test "testBinaryOperatorAnd" (SrcString "1 and 2") TypeError
    , 
    Test "testBinaryOperatorAnd2" (SrcString "true and 2") TypeError
    , 
    Test "testBinaryOperatorOr" (SrcString "2 or 4") TypeError
    , 
    Test "testBinaryOperatorEq" (SrcString "2 = true") TypeError
    , 
    Test "testBinaryOperatorDiv" (SrcString "2 div true") TypeError
    , 
    Test "testLet" (SrcString "let n = 1 in true") TypeError
    , 
    Test "testLet2" (SrcString "let n = true in 1 + n") TypeError
    , 
    Test "testLet" (SrcString "let x = 5 in true") TypeError
    , 
    Test "testIf1" (SrcString "if 5 then true else false") TypeError
    , 
    Test "testIf1" (SrcString "if true then 5 else false") TypeError
    , 
    Test "testIf2" (SrcString "if 5 > 10 then true else 10") TypeError
    , 
    Test "testEvalAdd" (SrcString "input a b in a + b + b") (Eval[1,1] (Value 3))
    , 
    Test "testEvalSub" (SrcString "input a b c in a - b - c") (Eval[3,2,1] (Value 0))
    , 
    Test "testEvalMul" (SrcString "input x in x * x") (Eval[1] (Value 1))
    , 
    Test "testEvalDiv" (SrcString "input x in x div x") (Eval[10] (Value 1))
    , 
    Test "tetsEvalMod" (SrcString "input x in x mod 2") (Eval[10] (Value 0))    
    , 
    Test "testEvalEnum" (SrcString "input x in x") (Eval[10] (Value 10))
    , 
    Test "testTwoVars" (SrcString "input x y in x + y") (Eval[1,2] (Value 3))
    , 
    Test "testFromFIleSimpleInput" (SrcFile "fromFile1.pp6") (Eval[1,2,3] (Value 6))
    , 
    Test "testFromFIleSimpleIf" (SrcFile "fromFile2.pp6") (Eval[] (Value 0))
    , 
    Test "testFromFileOperatorsBindings" (SrcFile "fromFile3.pp6") (Eval[] (Value (-12)))
    , 
    Test "testNot" (SrcString "not 5") TypeError
    , 
    Test "testsCount" (SrcString "1 * 2 * 3 * 4") (Eval[] (Value 24))
    , 
    Test "testEApp" (SrcString "fun funkcja (x:int):int = 2+3 in 3") (Eval[] (Value 3))
    , 
    Test "testEApp2" (SrcString "fun funkcja (x:bool):bool = true in 5") (Eval[] (Value 5))
    , 
    Test "testEApp3" (SrcString "fun funkcja (x:int):bool = false in false") TypeError
    , 
    Test "testEApp4" (SrcString "fun funkcja (X:int):bool = true in 10") (Eval[] (Value 10))
    , 
    Test "testEApp6" (SrcString "fun funkcja (X:int):bool = true in funkcja(10)") TypeError
    , 
    Test "testEApp6" (SrcString "fun funkcja (X:int):int = 5 in funkcja(10)") (Eval[] (Value 5))
    , 
    Test "testEApp7" (SrcString "fun funkcja (X:int):int = x input x in funkcja(x)") 
                      (Eval[10] (Value 10))
    , 
    Test "testEApp8" (SrcString "fun funkcja (X:int):bool = 10 input x in funkcja(x)") TypeError
    , 
    Test "testEApp9" (SrcString "fun f1 (X:int):int = x+1 fun f2(x:int):int = x+1 input x in f1(x) + f2(x)") (Eval[10] (Value 22))
    , 
    Test "testFromFIleFunctions1" (SrcFile "fromFile4.pp6") (Eval[1,1] (Value 6))
    , 
    Test "testFromFIleFunctions2" (SrcFile "fromFile5.pp6") (Eval[1] (Value 120))
    , 
    Test "testFromFIleFib" (SrcFile "fromFile6.pp6") (Eval[5] (Value 5))
    , 
    Test "testFromFIleExp" (SrcFile "fromFile7.pp6") (Eval[3] (Value 6))
    , 
    Test "testFromFIleExpTail" (SrcFile "fromFile8.pp6") (Eval[3] (Value 6))
    , 
    Test "testLetApp" (SrcString "fun f(x:int):int = 1 in let x = f(1) in x + 1") 
              (Eval[] (Value 2))
    , 
    Test "testFstPair" (SrcString "fst (2, 3)") (Eval[] (Value 2))
    , 
    Test "testSndPair" (SrcString "snd (2, 3)") (Eval[] (Value 3))
    , 
    Test "testFstPair2" (SrcString "fun f(x:int):int = x*x input x in fst (f(x), x+1)") 
                  (Eval[2] (Value 4))
    , 
    Test "debugIf" (SrcString "input n in if n <= 1 then n else n - 1 ") (Eval[0] (Value 0))
    , 
    Test "debugIf2" (SrcString "input n in if n <= 1 then n else n - 1 ") (Eval[2] (Value 1))
    , 
    Test "debugFib" (SrcString "fun f(n:int):int = n  input n in f(n-1)") 
                      (Eval[1] (Value 0))
    , 
    Test "debugFib2" (SrcString "fun f(n:int):int = if n <= 1 then n\
      \ else f(n-1) + f(n-2) input n in f(n) ") (Eval[2] (Value 1))
    , 
    Test "testUnaryEApp" (SrcString "fun f(n:int):int = if n > 0 then n else -n + n input n in -f(n)") 
                           (Eval [-2] (Value (0)))
    , 
    Test "testUnaryEApp2" (SrcString "fun f(n:int):int = if n < 0 then n else -f(n-1) input n in -f(n)")
                            (Eval [3] (Value (1)))
    , 
    Test "testUnaryEAppBool" (SrcFile "fromFile9.pp6")
                            (Eval [-1] (Value (-1)))
    , 
    Test "testUnaryEAppBool2" (SrcFile "fromFile10.pp6")
                            (Eval [-1] (Value (1)))
    , 
    Test "testMAtch" (SrcFile "fromFile11.pp6") (Eval[1] (Value 1))
    , 
    Test "testWrongMatch" (SrcString "match 1 with | [] -> 0 |x::xs -> 1") TypeError
    , 
    Test "test2functions" (SrcFile "fromFile12.pp6") (Eval[1] (Value 1))
    , 
    Test "test2functions2" (SrcFile "fromFile13.pp6") (Eval[1] (Value 1))
    , 
    Test "testList" (SrcString "let x = 1::3::[]:int list in 1 ") (Eval[]  (Value 1))
    ,
    Test { testName    = "positivIntTest",
           testProgram = SrcString " fun f(x:unit): int = 123 in f()",
           testAnswer  = Eval [] (Value 123)
    },
    Test { testName    = "emptyListTest",
           testProgram = SrcString " fun f(x:int list): int = 1 in f([]:int list)",
           testAnswer  = Eval [] (Value 1)
    },
    Test { testName    = "listTest",
           testProgram = SrcString " fun f(x:bool list): int = 2 in f([true,false]:bool list)",
           testAnswer  = Eval [] (Value 2)
    },         
    Test { testName    = "testNegative",
           testProgram = SrcString "fun f(x:unit): int = -1 in f()",
           testAnswer  = Eval [] (Value (-1))
    },
    Test { testName    = "testLet",
           testProgram = SrcString "fun f(x:unit): int = let y=1 in y in f() ",
           testAnswer  = Eval [] (Value 1)
    },
    Test { testName    = "testIfTrueFalse",
           testProgram = SrcString "fun f(x:unit): int = if true or false then 1 else 0 in f()",
           testAnswer  = Eval [] (Value 1)
    },    
    Test { testName    = "testFn",
           testProgram = SrcString "input x y in fn(x:int)->fn(y:int)->x+y",
           testAnswer  = Eval [1, 2] (Value 3)
    },
    Test { testName    = "testFunFn",
           testProgram = SrcString "fun f(x:int):int = fn(x:int)->fn(y:int)->x + y input x y in f(x)" ,
           testAnswer  = Eval [1, 2] (Value 3)
    },
    Test { testName    = "testFunFn2",
           testProgram = SrcString "fun f(x:int):int = fn(x:int)->x + y input x y in f(x)" ,
           testAnswer  = Eval [1, 2] (Value 3)
    },
    Test { testName    = "testFunFn3",
           testProgram = SrcString "fun f(x:int):int*int = fn(x:int)->fn(y:int)->(x, y) input x y in fst(f(x))" ,
           testAnswer  = Eval [1, 2] (Value 1)
    },
    Test { testName    = "testFunTypeError",
           testProgram = SrcString "fun f(x:int):int = x input x in let f = x in f f" ,
           testAnswer  = TypeError
    },
    Test { testName    = "testFnLet",
           testProgram = SrcString "input y in let x = 42 in fn(y:int)->x+y" ,
           testAnswer  = Eval[1] (Value 43)
    },
    Test { testName    = "testNewFun",
           testProgram = SrcString "fun fl(x:int):int list = if x <=10 then x::fl(x+1) else 0 in f(0)" ,
           testAnswer  = TypeError
    },
    Test { testName    = "testNewFun2",
           testProgram = SrcString "fun fl(x:int):int list = if x <=10 then x::fl(x+1) else 0 in let y = f(0) in 1" ,
           testAnswer  = TypeError
    },
    Test {
          testName    = "testDebugLength",
          testProgram = SrcFile "testLen.pp6",
          testAnswer  = Eval[] (Value 3)
    },
    Test {
          testName    = "testFirstElem",
          testProgram = SrcFile "first_list_elem.pp6",
          testAnswer  = Eval[] (Value 1)
    },
    Test {
          testName    = "testEmpty",
          testProgram = SrcFile "testEmpty.pp6",
          testAnswer  = Eval[] (Value (-1))
    },
    Test {
          testName    = "testSecondElem",
          testProgram = SrcFile "second_elem.pp6",
          testAnswer  = Eval[] (Value 7)
    },
    Test {
          testName    = "testSumListElems",
          testProgram = SrcFile "testSum.pp6",
          testAnswer  = Eval[] (Value 15)
    },
    Test {
          testName    = "testGetNthElem1",
          testProgram = SrcFile "getN.pp6",
          testAnswer  = Eval[3] (Value 4)
    },
    Test {
          testName    = "testGetNthElem2",
          testProgram = SrcFile "getN.pp6",
          testAnswer  = Eval[0] (Value 1)
    },
    Test {
          testName    = "testMakeList",
          testProgram = SrcString "let l = [1]:int list in let ll = 1::l::[]:int list in 1",
          testAnswer  = Eval[] (Value 1)
    },
    Test {
          testName    = "testMakeList3",
          testProgram = SrcFile "makeListFun.pp6",
          testAnswer  = Eval[] (Value 2)
    },
    Test {
          testName    = "testFunctionInFunction",
          testProgram = SrcString "fun g(x:int):int = x*x fun f(x:int):int = x in f(g(2))",
          testAnswer  = Eval[] (Value 4)
    },
    Test {
          testName    = "testArrorFunction",
          testProgram = SrcString "fun execute(f:int):int = f input x in execute(fn(x:int)->x)",
          testAnswer  = Eval[1] (Value 4)
    },
    Test {
          testName    = "testFunctionTakingBinaryExpression",
          testProgram = SrcString "fun square(x:int):int = x * x \
                                   \in \
                                   \square(2 + 2)",
          testAnswer  = Eval[] (Value 16)
    },
    Test {
          testName   = "testFunctionTakeArrow",
          testProgram = SrcFile "testFunctionTakeArrow.pp6",
          testAnswer = Eval[7] (Value 21)
    },
    Test {
          testName   = "testMultiply",
          testProgram = SrcFile "testMultiply.pp6",
          testAnswer = Eval[3] (Value 15)
    },
    Test {
          testName    = "testArrowTakeTwoParams",
          testProgram = SrcString "let x = (1, 2) in fn(x:int*int) -> \
                                        \let a = fst x in \
                                          \let b = snd x in \
                                            \a * b",
          testAnswer  = Eval[] (Value 2)
    },
    Test {
          testName    = "testFunPseudoDict",
          testProgram = SrcFile "testFunctionDict.pp6",
          testAnswer  = Eval[] (Value 10000)
    },
    Test {
          testName    = "testMap",
          testProgram = SrcFile "testMap.pp6",
          testAnswer  = Eval[5] (Value 50)
    }

  ]
 