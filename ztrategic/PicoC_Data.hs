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

data PicoCDatatype = INT | CHAR deriving (Show, Data)

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