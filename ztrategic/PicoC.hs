{-# LANGUAGE DeriveDataTypeable #-}
module PicoC where 

import Data.Char
import Data.Maybe
import Data.Function
import Data.Data
import Data.List
import Data.Generics.Zipper
import Library.Ztrategic
import Library.StrategicData (StrategicData)
import Test.QuickCheck
import PicoC_Data
import PicoC_Parser
import PicoC_Unparser
import PicoC_Generator
import PicoC_Functions

--Refactoring & Optimizations ---------------------------------------------------------------------------------------------------------------------------------------------------------------

instr:: BlocoC -> Maybe BlocoC
instr ((ITE (ConstBool True) bc _) : t) = Just (bc++t)
instr ((ITE (ConstBool False) _ bc): t) = Just (bc++t)
instr _ = Nothing


merge_conditions :: BlocoC -> Maybe BlocoC
merge_conditions x =
    let grouped_ifs = groupBy sameThenExpr (filter isITE x)
        isITE inst = case inst of 
            ITE{} -> True 
            _     -> False 
        sameThenExpr (ITE _ expr1 _) (ITE _ expr2 _) = unparserINST_aux expr1 == unparserINST_aux expr2
        mergeITEs (ITE cond1 expr1 _) (ITE cond2 _ _) = ITE (Or cond1 cond2) expr1 []  -- Merge the ITEs keeping the first condition and then expression
        merged_ifs = map (foldr1 mergeITEs) grouped_ifs
        rest_instructions = filter (not . isITE) x
    in Just (rest_instructions ++ merged_ifs)

opt:: PicoC -> PicoC
opt p = 
    let pZipper = toZipper p
        Just opt_expr = applyTP (innermost optimized_expressions) pZipper
            where optimized_expressions = failTP `adhocTP` instr `adhocTP` expr
        Just consCond_expr = applyTP (full_tdTP consCond) opt_expr
            where consCond = idTP `adhocTP` merge_conditions     
        Just newP = applyTP (innermost optimized_instructions) consCond_expr
            where optimized_instructions = failTP `adhocTP` instr     
    in fromZipper newP


etiquetaVars:: PicoC -> PicoC
etiquetaVars p = 
        let pZipper = toZipper p
            Just newP = applyTP (full_tdTP step) pZipper
            step = idTP `adhocTP` etiquetaAtrib
        in fromZipper newP

etiqueta1:: Exp -> Maybe Exp 
etiqueta1 (Var s) = Just (Var ("v_" ++ s))
etiqueta1 e = Just e

etiquetaAtrib:: Inst -> Maybe Inst 
etiquetaAtrib (Atrib s x) = Just (Atrib ("v_" ++ s) x)
etiquetaAtrib e = Just e


-- EXAMPLES -----------------------------------------------------------------------------
--ast = Mul (Add (Const 3) (Const 0)) (Const 5)
--ast2 = (Add (Add (Const (-4)) (Const 4)) (Const 5))

example1 = pPicoC "int margem; margem = 15; while ( margem > 30) { margem = 4 * 23 + 3; margem = 0+5;}"
example2 = pPicoC "int margem; margem = 15-2; if ( margem > 30 + 3 && @false) then { margem = 4 * 23 + 3; } else { margem = 3+0; a = 3*0;}"
example3 = pPicoC "int n; int result; n = 5+5;    result = 1*0;while (n <= 3 ||  (@false && n == 10)) { result = result * (n + 1); n = n - 1*0; }"
example4 = pPicoC "int num; int isPrime; int i; num = 17; isPrime = 1; i = 2; while (i < num) { if (num == 2) then { isPrime = @true;} else { if (num / i == 0) then { isPrime = @false; } } i = i + 1; }"
example5 = pPicoC "x=0;if (seniority < 2) then {return = 0;} if (monthsDisabled > 12) then {return = 0;} if (abs <= 4) then {return = 5;}"

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