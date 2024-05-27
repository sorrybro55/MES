module PicoC_Unparser where 

import PicoC_Data

unparser:: PicoC -> String
unparser (PicoC insts) = f insts
    where   f [] = ""
            f [x] = unparserINST x 
            f (x:xs) = unparserINST x ++ f xs 

unparserINST:: Inst -> String 
unparserINST (Atrib str exp) = str ++ " = " ++ unparserEXP exp ++ "; "
unparserINST (ReturnString str) = "return " ++ str ++ "; "
unparserINST (ReturnInt val) = "return " ++ show val ++ "; "
unparserINST (ReturnBool False) = "return @false; "
unparserINST (ReturnBool True) = "return @true; "
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
unparserEXP (Not exp) = "!( " ++ unparserEXP exp ++ " )"
unparserEXP (Add exp1 exp2) = unparserEXP_aux exp1 ++ " + " ++ unparserEXP_aux exp2
unparserEXP (Sub exp1 exp2) = unparserEXP_aux exp1 ++ " - " ++ unparserEXP_aux exp2
unparserEXP (Mul exp1 exp2) = unparserEXP_aux exp1 ++ " * " ++ unparserEXP_aux exp2
unparserEXP (Div exp1 exp2) = unparserEXP_aux exp1 ++ " / " ++ unparserEXP_aux exp2
unparserEXP (And exp1 exp2) = unparserEXP_aux exp1 ++ " && " ++ unparserEXP_aux exp2
unparserEXP (Or exp1 exp2) = unparserEXP_aux exp1 ++ " || " ++ unparserEXP_aux exp2
unparserEXP (Gt exp1 exp2) = unparserEXP_aux exp1 ++ " > " ++ unparserEXP_aux exp2
unparserEXP (Ls exp1 exp2) = unparserEXP_aux exp1 ++ " < " ++ unparserEXP_aux exp2
unparserEXP (Eq exp1 exp2) = unparserEXP_aux exp1 ++ " == " ++ unparserEXP_aux exp2
unparserEXP (Neq exp1 exp2) = unparserEXP_aux exp1 ++ " != " ++ unparserEXP_aux exp2
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
unparserEXP_aux (Neq exp1 exp2)  = "( " ++ unparserEXP exp1 ++ " != " ++ unparserEXP exp2 ++ " )"
unparserEXP_aux (Ge exp1 exp2)  = "( " ++ unparserEXP exp1 ++ " >= " ++ unparserEXP exp2 ++ " )"
unparserEXP_aux (Le exp1 exp2)  = "( " ++ unparserEXP exp1 ++ " <= " ++ unparserEXP exp2 ++ " )"
unparserEXP_aux exp = unparserEXP exp