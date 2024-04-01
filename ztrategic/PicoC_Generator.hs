module PicoC_Generator where

import Test.QuickCheck
import PicoC_Data
import PicoC_Unparser

get_varName = elements ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]

instance Arbitrary PicoC where 
    arbitrary = do
        insts <- sized $ \n -> resize (n `div` 2) $ listOf genInst
        return (PicoC insts)

instance Arbitrary Inst where 
    arbitrary = genInst

instance Arbitrary PicoCDatatype where
    arbitrary = elements [INT, CHAR]

instance Arbitrary Exp where
    arbitrary = sized $ genExp
    

genInst = frequency [(1, do { 
                        f <- arbitrary :: Gen PicoCDatatype;
                        varName <- get_varName;
                        return (Init f varName) }),
                    (1, do {
                        a <- sized $ genExp;
                        return (Atrib "a" a) }),
                    (1, do {
                        a <- sized $ genExpBool;
                        b <- genInst;
                        return (While a [b]) }),
                    (1, do {
                        a <- sized $ genExpBool ;
                        b <- genInst;
                        c <- genInst;
                        return (ITE a [b] [c])})]

genExpBool s = frequency [(1, do { f <- arbitrary;
                        return (ConstBool f) }),
                    (s, do {
                        a <- genExpBool s';
                        b <- genExpBool s';
                        return (And a b) }),
                    (s, do {
                        a <- genExpBool s';
                        b <- genExpBool s';
                        return (Or a b) }),
                    (s, do {
                        a <- genExp s';
                        b <- genExp s';
                        return (Gt a b) }),
                    (s, do {
                        a <- genExp s';
                        b <- genExp s';
                        return (Ls a b) }),
                    (s, do {
                        a <- genExp s';
                        b <- genExp s';
                        return (Eq a b)} ),
                    (s, do {
                        a <- genExp s';
                        b <- genExp s';
                        return (Neq a b)} ),
                    (s, do {
                        a <- genExp s';
                        b <- genExp s';
                        return (Ge a b)} ),
                    (s, do {
                        a <- genExp s';
                        b <- genExp s';
                        return (Le a b)} ),
                    (s, do {
                        a <- genExpBool s';
                        return (Not a)} )]
        where s' = s `div` 2

genExpInt s = frequency [(1, do { f <- arbitrary;
                            return (ConstInt f)} ),
                        (s, do {
                            a <- genExpInt s';
                            b <- genExpInt s';
                            return (Add a b)} ),
                        (s, do {
                            a <- genExpInt s';
                            b <- genExpInt s';
                            return (Sub a b)} ),
                        (s, do {
                            a <- genExpInt s';
                            b <- genExpInt s';
                            return (Mul a b)} ),
                        (s, do {
                            a <- genExpInt s';
                            b <- genExpInt s';
                            return (Div a b)} ),
                        (s, do {
                            a <- genExpInt s';
                            return (Neg a)} )]
            where s' = s `div` 2

genExp s = frequency [(1, do { f <- get_varName;
                            return (Var f)} ),
                        (s, do {
                            a <- genExpBool s;
                            return a }),
                        (s, do {
                            a <- genExpInt s;
                            return a })]
            where s' = s `div` 2