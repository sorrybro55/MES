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
example1 = pPicoC "while ( margem > 30) { margem = 4 * 23 + 3; margem = 0+5;} return margem;"
example2 = pPicoC "int margem; margem = 15-2; if ( margem > 30 + 3 && @false) then { margem = 4 * 23 + 3; } else { margem = 3+0; a = 3*0;}"
example3 = pPicoC "int n; int result; n = 5+5;    result = 1*0;while (n <= 3 ||  (@false && n == 10)) { result = result * (n + 1); n = n - 1*0; }"
example4 = pPicoC "int num; int isPrime; int i; num = 17; isPrime = 1; i = 2; while (i < num) { if (num == 2) then { isPrime = @true;} else { if (num / i == 0) then { isPrime = @false; } } i = i + 1; }"
example5 = pPicoC "x=0;if (seniority < 2) then {if (senioritys > 122) then {return = 0;} if (senioritys > 10022) then {return = 0;}  } if (monthsDisabled > 12) then {return = 5;} if (abs <= 4) then {return = 5;} if (monthsDisabled > 12) then {return = 4;} if (abs <= 4) then {return = 4;}"

test1 = fst . last 
test2 = opt . test1
test3 = unparser . test1
test4 = unparser . test2
test5 = pPicoC . test3
test6 = pPicoC . test4

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