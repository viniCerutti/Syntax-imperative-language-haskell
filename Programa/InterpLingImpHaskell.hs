{-
    T1 - Programacao Funcional 2017/2
    Vinicius Cerutti
    Dimas Olympio
-}

module InterpLingImpHaskell where

import Store
-- Data que guarda as operacoes aritmeticas
data AritExp =  L Integer                | Sub AritExp AritExp |
                Add AritExp AritExp      | Mult AritExp AritExp|
                Div AritExp AritExp      | Mod' AritExp AritExp|
                Abs' AritExp             | V Char  deriving (Show,Eq,Ord)
            
-- Data que guarda as operacoes logicas
data BoolExp =  B Bool                | No BoolExp          |
                And' BoolExp BoolExp  | Or' BoolExp BoolExp |
                Great AritExp AritExp | Less AritExp AritExp|
                Equal AritExp AritExp  deriving (Show,Eq,Ord)
                
-- Data que guarda os comandos para nossa lingugagem
data Commands = Nop                          | Atrib Char AritExp                |
                Seq Commands Commands        | Choice BoolExp Commands Commands  | -- Choice seria um If then else
                While BoolExp Commands       | Dowhile  Commands BoolExp    deriving (Show)

evalBoolExp::BoolExp -> Store -> Bool
evalBoolExp (B val) _ = val -- testado
evalBoolExp (No exp1) store =  not (evalBoolExp exp1 store) -- testado
evalBoolExp (And' exp1 exp2) store = (evalBoolExp exp1 store) && (evalBoolExp exp2 store) -- testado
evalBoolExp (Or' exp1 exp2) store = (evalBoolExp exp1 store) || (evalBoolExp exp2 store) -- testado
evalBoolExp (Great exp1 exp2) store = (evalAritExp exp1 store) > (evalAritExp exp2 store) -- testado
evalBoolExp (Less exp1 exp2) store = (evalAritExp exp1 store) < (evalAritExp exp2 store) -- testado
evalBoolExp (Equal exp1 exp2) store = ( evalAritExp exp1 store) == (evalAritExp exp2 store) -- testado

evalAritExp:: AritExp-> Store -> Integer
evalAritExp (L val) _ = val -- testado
evalAritExp (V c) store = value store c -- testado
evalAritExp (Sub exp1 exp2) store = (evalAritExp exp1 store) - (evalAritExp exp2 store) -- testado
evalAritExp (Add exp1 exp2) store = (evalAritExp exp1 store) + (evalAritExp exp2 store) -- testado
evalAritExp (Mult exp1 exp2) store = (evalAritExp exp1 store) * (evalAritExp exp2 store) -- testado
evalAritExp (Div exp1 exp2) store  = (evalAritExp exp1 store) `div` (evalAritExp exp2 store) -- testado
evalAritExp (Mod' exp1 exp2) store = (evalAritExp exp1 store) `mod` (evalAritExp exp2 store) -- testado
evalAritExp (Abs' exp1) store = abs (evalAritExp exp1 store) -- testado

evalCommands::Commands -> Store -> Store
evalCommands (Nop) store = store
evalCommands (Atrib name val) store = update store name (evalAritExp val store)
evalCommands (Seq comd1 comd2) store =  (evalCommands comd2 (evalCommands comd1 store)) -- comd2 utiliza o store do comd1
evalCommands (Choice expbool comd1 comd2) store = evalCommands (funcChoice expbool comd1 comd2 store) store
evalCommands ( While expbool comd) store = funcLoop expbool comd store
evalCommands ( Dowhile comd expbool) store = funcLoop expbool comd store

funcChoice :: BoolExp -> Commands -> Commands -> Store -> Commands
funcChoice expBool comd1 comd2 store
    | evalBoolExp expBool store = comd1
    | otherwise = comd2

funcLoop :: BoolExp -> Commands -> Store -> Store
funcLoop expBool comd store
    | evalBoolExp expBool store =  funcLoop expBool comd command 
    | otherwise = store
        where
            command = evalCommands comd store

variables :: Commands -> Store -> IO()
variables prog store = putStrLn (formatText memIniVariables calcProgStore)
        where
            calcProgStore = evalCommands prog initial
            memIniVariables = findSimTerms store calcProgStore
            formatText:: Store -> Store -> String
            formatText storeI storeF = traceStr++"\nStoreIni = "++storeIni++"\n"++traceStr++"\nStoreFin = "++storeFin++"\n"++traceStr
                where
                    storeIni = show storeI
                    storeFin = show storeF
                    traceNumber = max (length storeIni) (length storeFin)
                    traceStr = copyChar '-' traceNumber

                    copyChar:: Char -> Int -> String
                    copyChar c num
                        | num <=0 = ""
                        |otherwise = copyChar c (num-1)++[c]

