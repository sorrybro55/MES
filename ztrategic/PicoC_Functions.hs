module PicoC_Functions where

import PicoC_Data
import Data.Maybe
import Data.Function
import Data.Data
import Data.List
import Data.Generics.Zipper
import Library.Ztrategic
import Library.StrategicData (StrategicData)
import PicoC_Unparser

eval:: Exp -> Int
eval e = eval_aux e []

eval_aux:: Exp -> [(String, Int)] -> Int
eval_aux (ConstInt i) _ = i
eval_aux (Var n) c = fromJust (lookup n c) 
eval_aux (Neg e) c = - (eval_aux e c)
eval_aux (Add e d) c = eval_aux e c + eval_aux d c
eval_aux (Sub e d) c = eval_aux e c - eval_aux d c
eval_aux (Mul e d) c = eval_aux e c * eval_aux d c
eval_aux (Div e d) c = eval_aux e c `div` eval_aux d c

expr:: Exp -> Maybe Exp
expr (Neg (ConstInt x))                = Just (ConstInt (-x))
expr (Neg (Neg x))                     = Just x
expr (Not (ConstBool x))               = Just (ConstBool (not x))
expr (Add e (ConstInt 0))              = Just e
expr (Add (ConstInt 0) d)              = Just d 
expr (Add (ConstInt x) (ConstInt y))   = Just (ConstInt (x+y))
expr (Mul (ConstInt 0) (ConstInt y))   = Just (ConstInt (0))
expr (Mul (ConstInt x) (ConstInt 0))   = Just (ConstInt (0))
expr (Mul (ConstInt 1) x)              = Just x
expr (Mul x (ConstInt 1))              = Just x
expr (Mul (ConstInt x) (ConstInt y))   = Just (ConstInt (x*y))
expr (Sub e d)                         = Just (Add e (Neg d))
expr (Not (Not x))                     = Just x
expr (And (ConstBool False) _ )        = Just (ConstBool False)
expr (And _ (ConstBool False))         = Just (ConstBool False)
expr (Or (ConstBool True) _)           = Just (ConstBool True)
expr (Or _ (ConstBool True))           = Just (ConstBool True)
expr _ = Nothing

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
 
merge_conditions' :: BlocoC -> Maybe BlocoC
merge_conditions' x =
    let grouped_ifs = groupBy sameThenExpr (filter isITE x)
        isITE inst = case inst of 
            ITE{} -> True 
            _     -> False 
        sameThenExpr (ITE _ expr1 _) (ITE _ expr2 _) = unparserINST_aux expr1 == unparserINST_aux expr2
        mergeITEs (ITE cond1 expr1 _) (ITE cond2 _ _) = ITE (Or cond1 cond2) expr1 []  -- Merge the ITEs keeping the first condition and then expression
        merged_ifs = map (foldr1 mergeITEs) grouped_ifs
        rest_instructions = filter (not . isITE) x
    in if null grouped_ifs then Nothing else Just (rest_instructions ++ merged_ifs)

opt':: PicoC -> PicoC
opt' p = 
    let pZipper = toZipper p
        Just newP = applyTP (once_tdTP optimized_expressions) pZipper
            where optimized_expressions = failTP `adhocTP` merge_conditions'
    in fromZipper newP

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