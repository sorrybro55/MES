{-# LANGUAGE DeriveDataTypeable #-}
module PicoC_Data where

import Data.Char
import Data.Maybe
import Data.Function
import Data.Data
import Data.List
import Data.Generics.Zipper
import Library.Ztrategic
import Library.StrategicData (StrategicData)

data PicoC = PicoC [Inst] deriving (Show, Data)

data Inst = Atrib String Exp
            | While Exp BlocoC
            | ITE Exp BlocoC BlocoC
            | Init PicoCDatatype String
            deriving (Show, Data)

data PicoCDatatype = INT | CHAR deriving (Show, Data, Eq)

type BlocoC = [Inst]

data Exp = ConstInt Int 
        | ConstBool Bool
        | Var String
        | Neg Exp
        | Add Exp Exp 
        | Sub Exp Exp
        | Mul Exp Exp 
        | Div Exp Exp
        | Not Exp
        | And Exp Exp
        | Or Exp Exp 
        | Gt Exp Exp 
        | Ls Exp Exp 
        | Eq Exp Exp
        | Neq Exp Exp
        | Ge Exp Exp
        | Le Exp Exp
        deriving (Show, Data)

--Instance--------------------------------------------------------------------------------------------------------------------------------------------------------------

instance StrategicData PicoC 

instance Eq PicoC where
        (PicoC insts1) == (PicoC insts2) = insts1 == insts2

instance Eq Inst where 
        (Atrib var1 exp1) == (Atrib var2 exp2) = (var1 == var2) && (exp1 == exp2)
        (While exp1 bc1) == (While exp2 bc2) = (exp1 == exp2) && (bc1 == bc2)
        (ITE exp1 then1 else1) == (ITE exp2 then2 else2) = (exp1 == exp2) && (then1 == then2) && (else1 == else2)
        (Init type1 var1) == (Init type2 var2) = type1 == type2 && (var1 == var2)

instance Eq Exp where
        (ConstInt x) == (ConstInt y) = x == y
        (ConstBool x) == (ConstBool y) = x == y
        (Var x) == (Var y) = x == y
        (Neg x) == (Neg y) = x == y
        (Add x1 y1) == (Add x2 y2) = (x1 == x2 && y1 == y2) || (x1 == y2 || x2 == y1)
        (Mul x1 y1) == (Mul x2 y2) = (x1 == x2 && y1 == y2) || (x1 == y2 || x2 == y1)
        (Sub x1 y1) == (Sub x2 y2) = (x1 == x2 && y1 == y2)
        (Div x1 y1) == (Div x2 y2) = (x1 == x2 && y1 == y2)
        (Not x) == (Not y) = x == y
        (And x1 y1) == (And x2 y2) = (x1 == x2 && y1 == y2) || (x1 == y2 || x2 == y1)
        (Or x1 y1) == (Or x2 y2) = (x1 == x2 && y1 == y2) || (x1 == y2 || x2 == y1)
        (Eq x1 y1) == (Eq x2 y2) = (x1 == x2 && y1 == y2) || (x1 == y2 || x2 == y1)
        (Neq x1 y1) == (Neq x2 y2) = (x1 == x2 && y1 == y2) || (x1 == y2 || x2 == y1)
        (Gt x1 y1) == (Gt x2 y2) = (x1 == x2 && y1 == y2)
        (Ls x1 y1) == (Ls x2 y2) = (x1 == x2 && y1 == y2)
        (Ge x1 y1) == (Ge x2 y2) = (x1 == x2 && y1 == y2)
        (Le x1 y1) == (Le x2 y2) = (x1 == x2 && y1 == y2)
        