module PicoC_Functions where

import PicoC_Data
import Data.Maybe

eval:: Exp -> [(String, Int)] -> Int
eval (ConstInt i) _ = i
eval (Var n) c = fromJust (lookup n c) 
eval (Neg e) c = - (eval e c)
eval (Add e d) c = eval e c + eval d c
eval (Sub e d) c = eval e c - eval d c
eval (Mul e d) c = eval e c * eval d c
eval (Div e d) c = eval e c `div` eval d c

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