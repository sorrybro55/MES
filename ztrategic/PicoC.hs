module PicoC where 

import Test.QuickCheck
import PicoC_Data
import PicoC_Parser
import PicoC_Unparser
import PicoC_Generator
import PicoC_Functions
import Control.Monad
import Data.Generics.Zipper
import Library.Ztrategic
import Library.StrategicData (StrategicData)

-- EXAMPLES -----------------------------------------------------------------------------
--"int margem; margem = 15; while ( margem > 30) { margem = 4 * 23 + 3; margem = 0+5;} return margem;"
--example1 = pPicoC "while ( margem > 30) { margem = 4 * 23 + 3; margem = 0+5;} return margem;"
--example2 = pPicoC "int margem; margem = 15-2; if ( margem > 30 + 3 && @false) then { margem = 4 * 23 + 3; } else { margem = 3+0; a = 3*0;}"
--example3 = pPicoC "int n; int result; n = 5+5;    result = 1*0;while (n <= 3 ||  (@false && n == 10)) { result = result * (n + 1); n = n - 1*0; }"
--example4 = pPicoC "int num; int isPrime; int i; num = 17; isPrime = 1; i = 2; while (i < num) { if (num == 2) then { isPrime = @true;} else { if (num / i == 0) then { isPrime = @false; } } i = i + 1; }"
--example5 = pPicoC ""

--Calculadora(x,y)
programa1 = parse "int result;\
            \if ( operation == 1 || @false ) then { result = x + y; }\
            \if ( operation == 2 ) then { result = x - y; }\
            \if ( operation == 3 && @true ) then { result = x * y; }\
            \if ( operation == 4 ) then { result = x / y; }\
            \return result;"

testSuitePrograma1 = [([("x", 3::Int), ("y", 2::Int), ("operation", 1::Int)], 5),
                        ([("x", 3::Int), ("y", 2::Int), ("operation", 2::Int)], 1),
                        ([("x", 3::Int), ("y", 2::Int), ("operation", 3::Int)], 6),
                        ([("x", 3::Int), ("y", 2::Int), ("operation", 4::Int)], 1)]
runTestSuitePrograma1 = runTestSuite programa1 testSuitePrograma1


--Fatorial(x)
programa2 = parse "int fac;\
            \int result;\
            \fac = 1;\
            \result = 1;\
            \while(n > 1) {\
            \   result = fac * n;\
            \   fac = result;\
            \   n = n - 1;\ 
            \}\
            \return result;"
testSuitePrograma2 = [([("n", 3::Int)], 6),
                        ([("n", 1::Int)], 1),
                        ([("n", 0::Int)], 1),
                        ([("n", 5::Int)], 120)]
runTestSuitePrograma2 = runTestSuite programa2 testSuitePrograma2


--Max3(x,y)
programa3 = parse "int max;\
            \if (x > y) then {\
            \    max = 1;\
            \}\
            \else {\
            \   if (y == 10) then {\
            \       max = 1;\
            \   }\
            \   else {\
            \       max = 2;\
            \   }\
            \}return max;"
testSuitePrograma3 = [([("x", 22::Int), ("y", 3::Int)], 1),
                        ([("x", 1::Int), ("y", 20::Int)], 2),
                        ([("x", 1::Int), ("y", 10::Int)], 1),
                        ([("x", 0::Int), ("y", 0::Int)], 2)]
runTestSuitePrograma3 = runTestSuite programa3 testSuitePrograma3


parse = fst . last . pPicoC
opt_parse = opt . parse
unparse = unparser . parse
opt_unparse = unparser . opt_parse

-- TESTING -----------------------------------------------------------------------------------------------
tester :: IO ()
tester = do
  samples <- sample' (arbitrary :: Gen PicoC)
  mapM_ (\exp -> do
            let optimized = opt exp
                unparsed = unparser exp
                unparsedOpt = unparser optimized
            putStrLn $ "Original: " ++ show exp ++ "\n"
            putStrLn $ "Unparsed: " ++ unparsed ++ "\n"
            putStrLn $ "Unparsed Optimized: " ++ unparsedOpt
            putStrLn "-------------------") samples

generatePico :: IO ()
generatePico = do
    sample <- generate (arbitrary :: Gen PicoC)
    let optimized = opt sample
        unparsed = unparser sample
        unparsedOpt = unparser optimized
    putStrLn $ "Original: " ++ show sample ++ "\n"
    putStrLn $ "Unparsed: " ++ unparsedOpt ++ "\n"

-- PROPERTIES -------------------------------------------------------------------------------------------- 

{-TODO
fix':: Eq a => (a -> a) -> a -> a
fix' f x | x == x' = x 
        | otherwise = fix' f x'
        where x' = f x 

opt':: PicoC -> PicoC
opt' = fix' opt

prop_opt_idempotent:: PicoC -> Bool 
prop_opt_idempotent e = opt' (opt' e) == opt' e 
-}

--ADD --------------------------------------------------------------------------------------------------------------------

prop_add_neutral :: Property
prop_add_neutral = forAll (genExpInt_Add 1) $ \e ->
    eval e == eval (Add e (ConstInt 0)) && eval e == eval (Add (ConstInt 0) e)

prop_add_comm :: Exp -> Exp -> Property
prop_add_comm x y = forAll (liftM2 (,) (genExpInt_Add 1) (genExpInt_Add 1)) $ \(e1, e2) ->
    eval (Add e1 e2) == eval (Add e2 e1)

prop_mul_neutral :: Property
prop_mul_neutral = forAll (genExpInt_Mul 1) $ \e ->
    eval e == eval (Mul e (ConstInt 1)) && eval e == eval (Mul (ConstInt 1) e)

prop_mul_comm :: Exp -> Exp -> Property
prop_mul_comm x y = forAll (liftM2 (,) (genExpInt_Mul 1) (genExpInt_Mul 1)) $ \(e1, e2) ->
    eval (Mul e1 e2) == eval (Mul e2 e1)

prop_validAST:: PicoC -> Property
prop_validAST pico = (opt_parse (unparser pico)) === pico

mutateBinaryExp:: Exp -> Exp -> Gen Exp
mutateBinaryExp e1 e2 = elements [Add e1 e2, Sub e1 e2, Mul e1 e2, Div e1 e2,
 Gt e1 e2, Ls e1 e2, Ge e1 e2, Le e1 e2, And e1 e2, Or e1 e2, Eq e1 e2, Neq e1 e2]

mutateExp:: Exp -> Gen Exp
mutateExp (Add e1 e2) = mutateBinaryExp e1 e2
mutateExp (Sub e1 e2) = mutateBinaryExp e1 e2
mutateExp (Mul e1 e2) = mutateBinaryExp e1 e2
mutateExp (Div e1 e2) = mutateBinaryExp e1 e2
mutateExp (And e1 e2) = mutateBinaryExp e1 e2
mutateExp (Or e1 e2) = mutateBinaryExp e1 e2
mutateExp (Gt e1 e2) = mutateBinaryExp e1 e2
mutateExp (Ls e1 e2) = mutateBinaryExp e1 e2
mutateExp (Eq e1 e2) = mutateBinaryExp e1 e2
mutateExp (Neq e1 e2) = mutateBinaryExp e1 e2
mutateExp (Ge e1 e2) = mutateBinaryExp e1 e2
mutateExp (Le e1 e2) = mutateBinaryExp e1 e2

genPicoMutation:: PicoC -> Gen PicoC
genPicoMutation pico = do 
    chosen <- elements (getExps pico)
    mutatedExp <- mutateExp chosen
    let pZipper = toZipper pico
        Just newP = applyTP (once_tdTP step) pZipper
            where step = failTP `adhocTP` (applyMutation (chosen, mutatedExp))
    return (fromZipper newP)

applyMutation:: (Exp, Exp) -> Exp -> Maybe Exp
applyMutation (original, mutated) exp 
    | exp == original = Just mutated
    | otherwise = Nothing

getExps:: PicoC -> [Exp]
getExps pico = 
    let pZipper = toZipper pico 
        Just newP = applyTU (full_tdTU step) pZipper
            where step = failTU `adhocTU` groupExp
    in newP

groupExp:: Exp -> Maybe [Exp]
groupExp e = Just [e]

instrumentation:: PicoC -> PicoC
instrumentation (PicoC inst) = PicoC (aux inst)
    where 
        aux [] = []
        aux (i:is) = Print ("Executing: " ++ show i) : i : aux is

instrumentedTestSuite :: PicoC -> [(Inputs, Int)] -> Bool
instrumentedTestSuite program tests =
    all (\(inputs, expected) -> evaluate (instrumentation program) inputs == expected) tests

runTestSuiteMutation:: PicoC -> [(Inputs, Int)] -> Gen Bool
runTestSuiteMutation pico [] = return True
runTestSuiteMutation pico (h:t) = do
    mutatedPico <- genPicoMutation pico
    let (inputs, expected) = h
        result =  runTest mutatedPico (inputs, expected)
    if result 
        then runTestSuiteMutation mutatedPico t
        else return False

running :: IO ()
running = do
    result <- generate (runTestSuiteMutation programa1 testSuitePrograma1)
    putStrLn $ "Resultado dos testes com mutações: " ++ show result