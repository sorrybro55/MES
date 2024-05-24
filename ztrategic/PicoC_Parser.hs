module PicoC_Parser where 

import Parser
import Data.Char
import Data.Maybe
import Data.Function
import Data.Data
import Data.List
import PicoC_Data

import Prelude hiding ((<*>), (<$>))

pExpBool = (\x _ _ z -> And x z) <$> pExpRel <*> symbol '&' <*> symbol' '&' <*> pExpBool
        <|> (\x _ _ z -> Or x z) <$> pExpRel <*> symbol '|' <*> symbol' '|' <*> pExpBool
        <|> (\x -> x) <$> pExpRel

pExpRel = (\x _ z -> Gt x z) <$> pExpAdd_Sub <*> symbol' '>' <*> pExpRel
    <|> (\x _ z -> Ls x z) <$> pExpAdd_Sub <*> symbol' '<' <*> pExpRel
    <|> (\x _ _ z -> Eq x z) <$> pExpAdd_Sub <*> symbol' '=' <*> symbol' '=' <*> pExpRel
    <|> (\x _ _ z -> Neq x z) <$> pExpAdd_Sub <*> symbol' '!' <*> symbol' '=' <*> pExpRel
    <|> (\x _ _ z -> Le x z) <$> pExpAdd_Sub <*> symbol' '<' <*> symbol' '=' <*> pExpRel
    <|> (\x _ _ z -> Ge x z) <$> pExpAdd_Sub <*> symbol' '>' <*> symbol' '=' <*> pExpRel
    <|> (\x -> x) <$> pExpAdd_Sub

pExpAdd_Sub = (\x y z -> Add x z) <$> pExpDiv_Mul <*> symbol' '+' <*> pExpAdd_Sub
    <|> (\x y z -> Sub x z) <$> pExpDiv_Mul <*> symbol' '-' <*> pExpAdd_Sub
    <|> (\x -> x) <$> pExpDiv_Mul

pExpDiv_Mul = (\x y z -> Div x z) <$> pFator <*> symbol' '/' <*> pExpDiv_Mul
    <|> (\x y z -> Mul x z) <$> pFator <*> symbol' '*' <*> pExpDiv_Mul
    <|> (\x -> x) <$> pFator

pFator = (\x -> Var x) <$> pNomes
    <|>  (\x -> x) <$> pDataType

pDataType = (\x -> ConstInt x) <$> pInt
        <|> (\x -> ConstBool x) <$> pBool
        <|> (\x -> x) <$> enclosedBy (symbol' '(') pExpBool (symbol' ')')

pBlocoC = enclosedBy (symbol' '{')
                     (oneOrMore pInst)
                     (symbol' '}')

pInit =     f <$> token' "int" <*> oneOrMore (satisfy isSpace) <*> pNomes <*> symbol' ';'
        <|> g <$> token' "char" <*> oneOrMore (satisfy isSpace) <*> pNomes <*> symbol' ';'
    where f _ _ x _ = Init INT x
          g _ _ x _ = Init CHAR x  

pAtrib =    f <$> pNomes <*> symbol' '=' <*> pExpAdd_Sub <*> symbol' ';' 
    where f a b c  _ = Atrib a c 

pWhile = f <$> token' "while" <*> enclosedBy (symbol' '(') pExpBool (symbol' ')') <*> pBlocoC
    where f a b c = While b c 

pITE =  g <$> token' "if" <*> enclosedBy (symbol' '(') pExpBool (symbol' ')') 
                        <*> token' "then" <*> pBlocoC
    <|> f <$> token' "if" <*> enclosedBy (symbol' '(') pExpBool (symbol' ')') 
                        <*> token' "then" <*> pBlocoC
                        <*> token' "else" <*> pBlocoC
    where f _ x _ y _ z = ITE x y z
          g _ x _ y = ITE x y []

pReturn = (\_ x _ -> ReturnInt x)  <$> token' "return" <*> pInt <*> symbol' ';'
    <|>   (\_ x _ -> ReturnBool x) <$> token' "return" <*> pBool <*> symbol' ';'
    <|>   (\_ x _ -> ReturnString x) <$> token' "return" <*> pNomes <*> symbol' ';'


pInst = pAtrib 
    <|> pWhile
    <|> pITE
    <|> pInit
    <|> pReturn

pPicoC = f <$> zeroOrMore pInst
        where f a = PicoC a