{-# LANGUAGE DeriveDataTypeable #-}
module PicoC where 

import Parser
import Data.Char
import Data.Maybe
import Data.Function
import Data.Data
import Data.List
import Data.Generics.Zipper
import Library.Ztrategic
import Library.StrategicData (StrategicData)

import Prelude hiding ((<*>), (<$>))

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
        | Ge Exp Exp
        | Le Exp Exp
        deriving (Show, Data)

--Instance--------------------------------------------------------------------------------------------------------------------------------------------------------------

instance StrategicData PicoC 

--Parser ---------------------------------------------------------------------------------------------------------------------------------------------------------------

pExpBool = (\x _ _ z -> And x z) <$> pExpRel <*> symbol '&' <*> symbol' '&' <*> pExpBool
        <|> (\x _ _ z -> Or x z) <$> pExpRel <*> symbol '|' <*> symbol' '|' <*> pExpBool
        <|> (\x -> x) <$> pExpRel

pExpRel = (\x _ z -> Gt x z) <$> pExpAdd_Sub <*> symbol' '>' <*> pExpRel
    <|> (\x _ z -> Ls x z) <$> pExpAdd_Sub <*> symbol' '<' <*> pExpRel
    <|> (\x _ _ z -> Eq x z) <$> pExpAdd_Sub <*> symbol' '=' <*> symbol' '=' <*> pExpRel
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


pInst = pAtrib 
    <|> pWhile
    <|> pITE
    <|> pInit 

pPicoC = f <$> zeroOrMore pInst
        where f a = PicoC a

whileEXP = pWhile "while (a) { b=a; c = b ;}"
ifEXP = pITE "if ( c ) then { a = 10*c;} else {c=300;}"
picoExp = pPicoC "while ( margem > 30 ) { b=a; c = b ; if ( c ) then { a = 10;} else {c=300            ;}}"

-- Unparser / pretty printing ---------------------------------------------------------------------------------------------------------------------------------------------------------------

--unparser:: PicoC -> String 
unparser:: PicoC -> String
unparser (PicoC insts) = f insts
    where   f [] = ""
            f [x] = unparserINST x 
            f (x:xs) = unparserINST x ++ f xs 

unparserINST:: Inst -> String 
unparserINST (Atrib str exp) = str ++ " = " ++ unparserEXP exp ++ "; "
unparserINST (Init INT str) = "int " ++ str ++ "; "
unparserINST (Init CHAR str) = "char " ++ str ++ "; "
unparserINST (While exp bc) = "while ( " ++ unparserEXP exp ++ " ){ " ++ unparserINST_aux bc ++ " }"
unparserINST (ITE exp bc1 []) = "if ( " ++ unparserEXP exp ++ " ) then { " ++ unparserINST_aux bc1 ++ " }"
unparserINST (ITE exp bc1 bc2) = "if ( " ++ unparserEXP exp ++ " ) then { " ++ unparserINST_aux bc1 ++ " } else { " ++ unparserINST_aux bc2 ++ " }"

unparserINST_aux:: [Inst] -> String 
unparserINST_aux [] = ""
unparserINST_aux [x] = unparserINST x
unparserINST_aux (x:xs) = unparserINST x ++ unparserINST_aux xs

unparserEXP:: Exp -> String
unparserEXP (ConstInt c) = show c
unparserEXP (ConstBool True) = "@true"
unparserEXP (ConstBool False) = "@false"
unparserEXP (Var str) = str
unparserEXP (Neg exp) = "-( " ++ unparserEXP exp ++ " )"
unparserEXP (Add exp1 exp2) = unparserEXP_aux exp1 ++ " + " ++ unparserEXP_aux exp2
unparserEXP (Sub exp1 exp2) = unparserEXP_aux exp1 ++ " - " ++ unparserEXP_aux exp2
unparserEXP (Mul exp1 exp2) = unparserEXP_aux exp1 ++ " * " ++ unparserEXP_aux exp2
unparserEXP (Div exp1 exp2) = unparserEXP_aux exp1 ++ " / " ++ unparserEXP_aux exp2
unparserEXP (And exp1 exp2) = unparserEXP_aux exp1 ++ " && " ++ unparserEXP_aux exp2
unparserEXP (Or exp1 exp2) = unparserEXP_aux exp1 ++ " || " ++ unparserEXP_aux exp2
unparserEXP (Gt exp1 exp2) = unparserEXP_aux exp1 ++ " > " ++ unparserEXP_aux exp2
unparserEXP (Ls exp1 exp2) = unparserEXP_aux exp1 ++ " < " ++ unparserEXP_aux exp2
unparserEXP (Eq exp1 exp2) = unparserEXP_aux exp1 ++ " == " ++ unparserEXP_aux exp2
unparserEXP (Ge exp1 exp2) = unparserEXP_aux exp1 ++ " >= " ++ unparserEXP_aux exp2
unparserEXP (Le exp1 exp2) = unparserEXP_aux exp1 ++ " <= " ++ unparserEXP_aux exp2

unparserEXP_aux:: Exp -> String
unparserEXP_aux (Add exp1 exp2) = "( " ++ unparserEXP exp1 ++ " + " ++ unparserEXP exp2 ++ " )"
unparserEXP_aux (Sub exp1 exp2) = "( " ++ unparserEXP exp1 ++ " - " ++ unparserEXP exp2 ++ " )"
unparserEXP_aux (Mul exp1 exp2) = "( " ++ unparserEXP exp1 ++ " * " ++ unparserEXP exp2 ++ " )"
unparserEXP_aux (Div exp1 exp2) = "( " ++ unparserEXP exp1 ++ " / " ++ unparserEXP exp2 ++ " )"
unparserEXP_aux (And exp1 exp2) = "( " ++ unparserEXP exp1 ++ " && " ++ unparserEXP exp2 ++ " )"
unparserEXP_aux (Or exp1 exp2)  = "( " ++ unparserEXP exp1 ++ " || " ++ unparserEXP exp2 ++ " )"
unparserEXP_aux (Gt exp1 exp2)  = "( " ++ unparserEXP exp1 ++ " > " ++ unparserEXP exp2 ++ " )"
unparserEXP_aux (Ls exp1 exp2)  = "( " ++ unparserEXP exp1 ++ " < " ++ unparserEXP exp2 ++ " )"
unparserEXP_aux (Eq exp1 exp2)  = "( " ++ unparserEXP exp1 ++ " == " ++ unparserEXP exp2 ++ " )"
unparserEXP_aux (Ge exp1 exp2)  = "( " ++ unparserEXP exp1 ++ " >= " ++ unparserEXP exp2 ++ " )"
unparserEXP_aux (Le exp1 exp2)  = "( " ++ unparserEXP exp1 ++ " <= " ++ unparserEXP exp2 ++ " )"
unparserEXP_aux exp = unparserEXP exp

----
eval:: Exp -> [(String, Int)] -> Int
eval (ConstInt i) _ = i
eval (Var n) c = fromJust (lookup n c) 
eval (Neg e) c = - (eval e c)
eval (Add e d) c = eval e c + eval d c
eval (Sub e d) c = eval e c - eval d c
eval (Mul e d) c = eval e c * eval d c
eval (Div e d) c = eval e c `div` eval d c


{-
ex3 = let t = toZipper examplePicoC
        Just f1 = down' t
        Just attrib = down' f1 
        Just rattrib = right attrib
        Just ite = down' rattrib
        {...}
        in getHole attrib :: Maybe Inst 
-}

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


--if_test = [Atrib "x" (ConstInt 0),ITE (Ls (Var "seniority") (ConstInt 2)) [Atrib "return" (ConstInt 2)] [],ITE (Gt (Var "monthsDisabled") (ConstInt 12)) [Atrib "return" (ConstInt 0)] [],ITE (Eq (Var "isPartTime") (ConstInt 0)) [Atrib "return" (ConstInt 0)] []]

{-
f:: BlocoC -> Maybe BlocoC
f x =
    let filtered_ifs = filter isITE x
        isITE inst = case inst of 
                ITE{} -> True 
                _     -> False 
        reduced_ifs = foldl function_aux (head filtered_ifs) (tail filtered_ifs)
            where function_aux:: Inst -> Inst -> BlocoC
                  function_aux (ITE cond1 expr1 []) (ITE cond2 expr2 []) = if unparserINST_aux expr1 == unparserINST_aux expr2
                                                                                then [(ITE (Or cond1 cond2) expr1 [])] 
                                                                                else [(ITE cond1 expr1 []):(ITE cond2 expr2 [])]
    in Just reduced_ifs
    -}