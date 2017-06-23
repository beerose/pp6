{-# LANGUAGE Safe #-}

module AleksandraSikora where

import           AST
import           DataTypes
import qualified Data.Map  as Map
import           Data.Either


-- srodowisko przechowujace funkcje
type ArrowExpression p = Expr p
type FunctionEnvironment p = Map.Map Var (Either (FunctionDef p) (ArrowExpression p))

-- definicja typu Error
type Error p = TypeCheckResult p
type RuntimeError = EvalResult


-- typ Value uzyty w funkcji inferEval
data Value = VInteger Integer | VBool Bool | VUnit | VPair (Value, Value) | VList [Value] 
      deriving (Show, Eq, Ord)

-- srodowisko przechowujace wartosci zmiennych
type ValueEnviroment = Map.Map Var Value
-- srodowisko przechowujace typy zmiennych
type TypeEnviroment = Map.Map Var Type

-- pomocnicze typy
type VUnit = ()


-- listy pomocnicze, przechowujace rozne typy operatorow binarnych

boperatorsBoolean    = [BAnd, BOr]
boperatorsComparison = [BEq, BNeq, BLt, BGt, BLe, BGe]
boperatorsArithmetic = [BAdd, BSub, BMul, BDiv, BMod]

-- glowna funckcja oblciczajaca cale programy
eval :: [FunctionDef p] -> [(Var,Integer)] -> Expr p -> EvalResult
eval funcs vars expr = case inferEval fenv env expr of
                        Left _ -> RuntimeError
                        Right result  -> case result of
                              VInteger int -> Value int
                              _ -> RuntimeError
      where env = Map.fromList $ map (\(v, i) -> (v, VInteger i)) vars  
            fenv = Map.fromList $ zip (map funcName funcs) (map Left funcs)

-- sprawdza czy funkcja jest w srodowisku                                                               
fncLookup :: FunctionEnvironment p -> Var -> Maybe (Either (FunctionDef p) (ArrowExpression p))
fncLookup fenv fsym = Map.lookup fsym fenv

-- funkcja zwracajaca wartosc wyrazen 
inferEval :: FunctionEnvironment p -> ValueEnviroment -> Expr p -> Either (Error p) Value
inferEval fenv valEnv expr = case expr of
      ENum    _ val                   -> Right $ VInteger val
      EBool   _ bool                  -> Right $ VBool bool
      EVar    _ var                   -> case Map.lookup var valEnv of
                                            Just t -> Right t
                                            Nothing -> Right $ VInteger 0
      EUnit   _                       -> Right $ VUnit
      ENil    _ _                     -> Right $ VList []
      EApp    p expr1    expr2        -> case expr1 of
            EVar p var -> case Map.lookup var fenv  of
                        Just func -> case func of
                              Left def -> inferEval fenv valEnv (ELet p (funcArg def) expr2 (funcBody def))
                              Right arr -> undefined
                        Nothing  -> Left $ Error p "Function not in function environment."
            _ -> undefined
      EPair   _ expr1    expr2        -> evalEpair (fenv, valEnv, expr1, expr2)
      EFst    p exprFst               -> evalEFst (p, fenv, valEnv, exprFst)
      ESnd    p exprSnd               -> evalESnd (p, fenv, valEnv, exprSnd)
      ECons   p expr1    expr2        -> evalECons (p, fenv, valEnv, expr1, expr2)
      EUnary  _ operator expr1        -> evalEUnary (fenv, valEnv, operator, expr1)
      EBinary p operator expr1 expr2  -> evalEBinary (p, fenv, valEnv, operator, expr1, expr2)
      EIf     p expr1    expr2 expr3  -> evalEif (p, fenv, valEnv, expr1, expr2, expr3)
      ELet    p var      expr1 expr2  -> case inferEval fenv valEnv expr1 of
            Left err -> Left $ Error p "Let error."
            Right innerValue -> case expr2 of
                  EApp p e1 e2 -> case e1 of
                        EVar p v -> case Map.lookup v valEnv of
                              Just vv -> Right vv
                              Nothing -> case Map.lookup v fenv of
                                    Just def -> case def of
                                          Left fun -> inferEval fenv (Map.insert var innerValue valEnv) (ELet p (funcArg fun) e2 (funcBody fun))
                                          Right arr -> undefined
                                    Nothing -> Left $ Error p "Not a function."
                        _ -> Left $ Error p " " 
                  _ -> inferEval fenv (Map.insert var innerValue valEnv) expr2
      EMatchL p exprToMatch exprN (var1, var2, exprC) -> evalEmatchL (p, fenv, valEnv, exprToMatch, exprN, (var1, var2, exprC))         
      EFn     p var typ expr          -> inferEval fenv valEnv expr 

-- funckja ewaluujaca pare
evalEpair :: (FunctionEnvironment p, ValueEnviroment, Expr p, Expr p) -> Either (Error p) Value
evalEpair (fenv, valEnv, expr1, expr2) = case val1 of
                  Left err -> Left err
                  Right someValue -> case val2 of
                              Left err -> Left err
                              Right anotherValue -> Right $ VPair (someValue, anotherValue)
            where val1 = inferEval fenv valEnv expr1
                  val2 = inferEval fenv valEnv expr2

-- funkcja zwraca pierwsze wyrazenie z pary
evalEFst :: (p, FunctionEnvironment p, ValueEnviroment, Expr p) -> Either (Error p) Value
evalEFst (p, fenv, valEnv, exprFst) = case inferEval fenv valEnv exprFst of
                                    Right (VPair (t1, _)) -> Right t1 
                                    _ -> Left $ Error p "Not pair in EFst expression."

-- funckja zwraca drugie wyrazenie z pary
evalESnd :: (p, FunctionEnvironment p, ValueEnviroment, Expr p) -> Either (Error p) Value
evalESnd (p, fenv, valEnv, exprSnd) = case inferEval fenv valEnv exprSnd of
                                    Right (VPair (_, t2)) -> Right t2 
                                    _ -> Left $ Error p "Not pair in ESnd expression."
                                    
-- funkcja konstruujaca liste
evalECons :: (p, FunctionEnvironment p, ValueEnviroment, Expr p, Expr p) -> Either (Error p) Value
evalECons (p, fenv, valEnv, expr1, expr2) = case valHeadExpr of
            Left err -> Left err
            Right valE1 -> case expr2 of
                                    ECons _ _ _  -> Right $ VList ((valE1 : valTailExpr) ++ [])
                                    ENil _ _  -> Right $ VList $ (valE1 : [])
                                    _ -> Left $ Error p "Error with evaluating lists."
            where valHeadExpr = inferEval fenv valEnv expr1
                  valTailExpr = case inferEval fenv valEnv expr2 of
                              Right val -> case val of
                                    VList [] -> []
                                    VList vasasu -> vasasu
                                    _ -> undefined
                              Left err  -> undefined

-- funkcja obliczajaca wyrazenie po dopasowaniu do wzorca
evalEmatchL :: (p, FunctionEnvironment p, ValueEnviroment, Expr p, Expr p, (Var, Var, Expr p)) -> Either (Error p) Value
evalEmatchL (p, fenv, valEnv, exprToMatch, exprN, (head, tail, exprC)) = case exprToMatch of
            EVar p var -> case Map.lookup var valEnv  of
                  Just val -> case val of
                        VList l@(h:t) -> inferEval fenv (Map.insert head h (Map.insert tail (VList t) valEnv)) exprC                  
                        VList [] -> inferEval fenv valEnv exprN
                        _ -> Left $ Error p "Not list in pattern matching expression."
                  Nothing -> Left $ Error p "Not list in pattern matching expression."
            _ -> Left $ Error p "Error with pattern matching"

-- funckja obliczajaca wyrazenia unarne
evalEUnary :: (FunctionEnvironment p,  ValueEnviroment, UnaryOperator, Expr p) -> Either (Error p) Value
evalEUnary (fenv, valEnv, operator, expr1) = case inferEval fenv valEnv expr1 of
            Left err -> Left err 
            Right innerValue -> case operator of
                                    UNeg -> Right (let VInteger v = innerValue in VInteger $ negate v)
                                    UNot -> Right (let VBool v = innerValue in VBool $ not v) 

-- funkcja obliczajaca wyrazenia binarne
evalEBinary :: (p, FunctionEnvironment p, ValueEnviroment, BinaryOperator, Expr p, Expr p) -> Either (Error p) Value
evalEBinary (p, fenv, valEnv, operator, expr1, expr2) =
                        if operator `elem` boperatorsBoolean then evalBools p operator ev1 ev2
                        else  evalIntegers p operator ev1 ev2
                                          where ev1 = inferEval fenv valEnv expr1 
                                                ev2 = inferEval fenv valEnv expr2   
                                                                                                                  


-- funckja obliczajaca wyrazenia arytmetyczne
evalIntegers :: p -> BinaryOperator -> Either (Error p) Value -> Either (Error p) Value -> Either (Error p) Value
evalIntegers p op (Right (VInteger a)) (Right (VInteger b)) = case op of
      BEq  -> Right     $ VBool (a == b)
      BNeq -> Right     $ VBool (not(a == b))
      BLt  -> Right     $ VBool (a < b)
      BGt  -> Right     $ VBool (a > b)
      BLe  -> Right     $ VBool (a <= b)
      BGe  -> Right     $ VBool (a >= b)
      BAdd -> Right     $ VInteger (b + a) 
      BSub -> Right     $ VInteger (a - b)
      BMul -> Right     $ VInteger (a * b)
      BMod -> case b of
                  0   -> Left      $ Error p "Dividing by zero."
                  _   -> Right     $ VInteger (a `mod` b)
      BDiv -> case b of
                  0   -> Left      $ Error p "Dividing by zero."
                  _   -> Right     $ VInteger (a `div` b)
      _ -> Left $ Error p "Error with evaluating binary expression."

evalIntegers p _ _ _ = Left $ Error p "Runtime error in binary expresion."

-- funckja zwraca wartosc wyrazen z operatorami boolowskimi i operatorami porownania
evalBools :: p -> BinaryOperator -> Either (Error p) Value -> Either (Error p) Value -> Either (Error p) Value
evalBools p op (Right (VBool a)) (Right (VBool b)) = case op of
      BAnd -> Right     $ VBool (a && b)
      BOr  -> Right     $ VBool (a || b)
      _ -> Left $ Error p "Error with evaluating binary expression."

evalBools p _ _ _ = Left $ Error p "Runtime error in binary expresion."


-- funckja obliczajaca wyrazenia warunkowe
evalEif :: (p, FunctionEnvironment p, ValueEnviroment, Expr p, Expr p, Expr p) -> Either (Error p) Value
evalEif (p, fenv, valEnv, expr1, expr2, expr3) = case inferEval fenv valEnv expr1 of
      Left err -> Left err 
      Right innerValue -> case innerValue of
            VBool True  -> inferEval fenv valEnv expr2
            VBool False -> inferEval fenv valEnv expr3
            _ -> Left $ Error p "Error with evaluating If."




-- funckja ktora sprawdza czy caly program jest poprawny
typecheck :: [FunctionDef p] -> [Var] -> Expr p -> TypeCheckResult p
typecheck funcs vars expr = case inferType fenv env expr of      
            Left err         -> err
            Right resultType -> if resultType == TInt 
                                    then Ok
                                    else case resultType of
                                          TList _ -> Error (getData expr) "Program returns list."
                                          _ -> Error (getData expr) "Program does not return int."
      where en = (Map.fromList $ zip vars $ repeat TInt )
            fe = Map.fromList $ zip (map funcName funcs) (map funcResType funcs)
            env = Map.union en fe
            fenv = Map.fromList $ zip (map funcName funcs) (map Left funcs)

-- sprawdzenie jaki jest typ zmiennej var srodowisku
envLookup :: TypeEnviroment -> Var -> Maybe Type
envLookup envt var = Map.lookup var envt




-- funkcja sprawdzajaca czy typ podany jako pierwszy argument zgadza sie 
-- z typem wyrazenia podanym jako drugi argument
checkInnerType :: Type -> Either (Error p) Type -> (p, String) -> Either (Error p) Type
checkInnerType expectedType x err = case x of
      Left oldError -> Left oldError
      Right innerT -> if innerT == expectedType 
            then Right expectedType 
            else let (outerP, s) = err in Left $ Error outerP s 

inferType :: FunctionEnvironment p -> TypeEnviroment -> Expr p -> Either (Error p) Type
inferType fenv envt expr = case expr of
  ENum    _ _                             -> Right TInt
  EBool   _ _                             -> Right TBool
  EVar    p var                           -> case envLookup envt var of
                                               Just t  -> Right t
                                               Nothing  -> case Map.lookup var fenv of
                                                     Just t -> case t of
                                                           Left app -> Right (funcResType app)
                                                           Right arr -> inferType fenv envt arr
                                                     Nothing -> Left $ Error p $ "No variable like " ++ show var ++ " in the TypeEnviroment."
  EUnit   _                               -> Right TUnit
  ENil    _ t                             -> Right $ TList t
  EFst    p exprFst                       -> case inferType fenv envt exprFst of
                                               Right (TPair t1 _) -> Right t1 
                                               _ -> Left $ Error p ("Not pair expresion in EFst." ++ show exprFst)
  ESnd    p exprSnd                       -> case inferType fenv envt exprSnd of
                                               Right (TPair _ t2) -> Right t2 
                                               _ -> Left $ Error p ("Not pair expresion in ESnd." ++ show exprSnd)
  ECons   p expr1     expr2               -> inferTypeECons (p, fenv, envt, expr1, expr2)
  EPair   _ exprPair1 exprPair2           -> inferTypeEPair (fenv, envt, exprPair1, exprPair2)
  EUnary  p operator  innerExpr           -> inferTypeEUnary (p, fenv, envt, operator, innerExpr)
  EBinary p operator  leftExpr rightExpr  -> inferTypeEBinary (p, fenv, envt, operator, leftExpr, rightExpr)  
  ELet    p var       expr1    expr2      -> case expr1 of
        -- sprawdzam czy expr1 nie jest wyrazeniem lambda,
        -- jesli tak to chce z nim postepowac inaczej
            EFn p v t e -> case inferType fenv envt expr1 of
                 Left oldError -> Left $ Error p "Error inferType ELet EFn."
                 Right fnT -> case fncLookup fenv var of
                       -- nie chce nadpisywac funkcji
                       Just t -> Left $ Error p "Var is already in environment."
                       Nothing -> case expr2 of
                             EApp p e1 e2 -> inferType fenv envt e1
                             _ ->  inferType fenv envt (ELet p var e expr2)
            _ -> case inferType fenv envt expr1 of
                 Left oldError -> Left oldError --Left $ Error p "Error ELet."
                 Right newType -> case fncLookup fenv var of
                       Just t -> Left $ Error p "Var is already in environment."
                       Nothing -> case expr2 of
                                   EApp p e1 e2 -> case e1 of
                                         EVar p v -> case envLookup envt v of
                                                Just typ -> Right typ
                                                Nothing -> Left $ Error p "Not function." 
                                          
                                   _ -> case inferType fenv (Map.insert var newType envt) expr2 of
                                                Left err -> Left err
                                                Right typ -> Right typ
                                                           
  EIf     p exprIf    exprThen exprElse   -> inferTypeEIf (p, fenv, envt, exprIf, exprThen, exprElse)
  EApp    p expr1     expr2            -> inferTypeEApp (p, fenv, envt, expr1, expr2)
  EMatchL p toMatch   exprN (var1, var2, exprC) -> inferTypeEMatch (p, fenv, envt, toMatch, exprN, (var1, var2, exprC))
  EFn     p var       typ      expr       -> case inferType fenv (Map.insert var typ envt) expr of
            Left err  -> Left err
            Right fnT -> Right fnT 


-- funkcja sprawdzajaca typ konstruktora listy
inferTypeECons :: (p, FunctionEnvironment p, TypeEnviroment, Expr p, Expr p) -> Either (Error p) Type
inferTypeECons (p, fenv, envt, expr1, expr2) = case lefts types of
        [] -> case inferType fenv envt expr2 of
            Right t2 -> case t2 of
                          TList _ -> case inferType fenv envt expr1 of
                                            Left err -> Left err
                                            Right t1 -> Right $ TList t1
                          _ -> Left $ Error p "Error with typecheking list." 
            _ -> Left $ Error p "Not list in second argument ECons."        
        l:_ -> Left l
      where types = map (inferType fenv envt) [expr1, expr2]

-- funcka sprawdza poprawnosc typow pary
inferTypeEPair :: (FunctionEnvironment p, TypeEnviroment, Expr p, Expr p) -> Either (Error p) Type
inferTypeEPair (fenv, envt, expr1, expr2) = case lefts types of
        [] -> Right $ TPair (rtypes !! 0) (rtypes !! 1)
        l:_ -> Left l
      where types = map (inferType fenv envt) [expr1, expr2]
            rtypes = rights types

-- funkcja zwracająca typ argumentow jakie przyjmuje operator binarny podany jako argument funkcji
checkBinaryOperatorInput :: BinaryOperator -> Type
checkBinaryOperatorInput operator = 
      if operator `elem` boperatorsBoolean then TBool
      else TInt

-- pomocnicza funkcja do obslugi operatorow binarnych
-- osobno rozpatruje przypadki kiedy jednym z wyrazen jest aplikacja funkcji, 
-- aby uniknac zapetlenia programu
inferTypeEBinary ::(p, FunctionEnvironment p, TypeEnviroment, BinaryOperator, Expr p, Expr p) -> Either (Error p) Type 
inferTypeEBinary (p, fenv, env, binaryOperator, left, right) = case left of
            EApp _ expr _ -> case expr of
                  EVar p var -> case Map.lookup var env of
                        Just typ -> Right typ
                        Nothing -> undefined
                  _ -> Left $ Error p "Function not in FuncyionEnvironment."

            _ -> case right of
                      EApp _ expr _ -> case expr of
                         EVar p var -> case Map.lookup var env of
                               Just typ -> Right typ
                               Nothing -> undefined
                         _ -> Left $ Error p "Function not in FuncyionEnvironment."
                        
                      _ ->  case sideType expectedInput left of
                                  Left oldError -> Left oldError  
                                  Right leftType -> case sideType leftType right of
                                          Left oldError -> Left oldError
                                          Right _ -> Right outerType
  where expectedInput = checkBinaryOperatorInput binaryOperator
        sideType expt side = checkInnerType expt (inferType fenv env side) (p, "Binary expresions of different types.")
        outerType = if binaryOperator `elem` boperatorsComparison then TBool else expectedInput

-- pomocnicza funkcja do obslugi operatorow unarnych
-- tak jak przy wyrazeniach binarnych osobno rozpatruje przypadek gdy wyrazeniem jest aplikacja funkcji
inferTypeEUnary :: (p, FunctionEnvironment p, TypeEnviroment, UnaryOperator, Expr p) -> Either (Error p) Type
inferTypeEUnary (p, fenv, outerEnv, operator, expr)
  | operator == UNot = 
        case expr of
            EApp _ expr _ -> case expr of
                  EVar p var -> case Map.lookup var outerEnv of
                        Just typ -> Right typ
                        Nothing -> undefined
                  _ -> Left $ Error p "Function not in FuncyionEnvironment."  
            _ -> checkInnerType TBool (inferType fenv outerEnv expr) (p, "Expression has not proper type - Bool.")
  | operator == UNeg = case expr of
            EApp _ expr _ -> case expr of
                  EVar p var -> case Map.lookup var outerEnv of
                        Just typ -> Right typ
                        Nothing -> undefined
                  _ -> Left $ Error p "Function not in FuncyionEnvironment." 
            _ -> checkInnerType TInt (inferType fenv outerEnv expr) (p, "Expression has not proper type - Int.")
  | otherwise = Left $ Error p "Unary Wrong Operator error."


-- funkcja sprawdzajaca czy wyrazenia expr1, expr2 z (if _ then expr1 else expr2)
-- sa tego samego typu 
eithersEqual :: p -> Type -> Type -> Either (Error p) Type
eithersEqual p typeThen typeElse = case typeThen of
      TList t -> Right t
      _ -> if typeElse == typeThen then Right typeThen
      else Left $ Error p  "Else statement type and then statement type not equal."

-- pomocnicza funkcja do obslugi wyrazenia If
inferTypeEIf :: (p, FunctionEnvironment p, TypeEnviroment, Expr p, Expr p, Expr p) -> Either (Error p) Type
inferTypeEIf (p, fenv, env, exprIf, exprThen, exprElse) =
    case inferType fenv env exprIf of
        Left oldError -> Left oldError
        Right ifType -> case ifType of
                            TBool -> case inferType fenv env exprThen of
                                        Left oldError -> Left oldError
                                        Right thenType -> case exprElse of
                                                EApp _ _ _-> Right thenType
                                                EMatchL p expr1 nil con -> inferType fenv env (EMatchL p expr1 nil con)
                                                _ -> case inferType fenv env exprElse of
                                                            Left oldError -> Left oldError
                                                            Right elseType -> eithersEqual p thenType elseType
                            _ -> Left $ Error p  "If statement not TBool."

-- funkcja sprawdzajaca poprawnosc wyrazenia aplikacji funkcji
inferTypeEApp :: (p, FunctionEnvironment p, TypeEnviroment, Expr p, Expr p) -> Either (Error p) Type
inferTypeEApp (p, fenv, envt, exprFunction, exprParam) = case exprFunction of
            EVar p var  -> case fncLookup fenv var of
                  Just something -> case something of
                        Left def -> case inferType fenv envt exprParam of
                              Left err -> Left $ Error p "Error with first expr in EApp."
                              Right newType ->  case inferType fenv envt (ELet p (funcArg def) exprParam (funcBody def)) of
                                                Left err -> Left $ Error p "here"
                                                Right exprType -> Right $ funcResType def
                        Right arr -> undefined
                  Nothing -> Left $ (Error p) "Function application error."
            EFn p var typ expr -> case inferType fenv envt expr of
                  Left err -> Left $ Error p "Error with EFn in EApp."
                  Right someType -> Left $ Error p $ "error."

-- funkcja sprawdzajaca poprawnosc wyrazenia dopasowania listy do wzorca
inferTypeEMatch :: (p, FunctionEnvironment p, TypeEnviroment, Expr p, Expr p,(Var, Var, Expr p)) -> Either (Error p) Type
inferTypeEMatch (p, fenv, envt, exprToMatch, nilclause, (var1, var2, exprcon)) = case exprToMatch of
           EVar p var -> case envLookup envt var of
                 Just (TList typ) -> case lefts types of
                              [] -> Right $ rtypes !! 0
                              l:ll -> Left $ l
                   where types = map (inferType fenv (Map.insert var1 typ (Map.insert var2 (TList typ) envt))) [nilclause, exprcon]
                         rtypes = rights types

                 _ -> Left $ Error p $ "Not list in EMatchL as first argument." -- robisz lookup i jeszcze raz wywołujesz inferTypeEMatch          
           _ -> Left $ Error p $ "Not list in EMatchL as first argument." 




