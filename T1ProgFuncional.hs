module T1ProgFuncional where

import Store
-- Data que guarda as operacoes aritmeticas
data AritExp =  L Integer                | Sub AritExp AritExp |
                Add AritExp AritExp      | Mult AritExp AritExp|
                Div AritExp AritExp      | V Char  deriving (Show,Eq,Ord)
            
-- Data que guarda as operacoes logicas
data BoolExp =  B Bool                | No BoolExp          |
                And' BoolExp BoolExp  | Or' BoolExp BoolExp |
                Great AritExp AritExp | Less AritExp AritExp|
                Equal AritExp AritExp  deriving (Show,Eq,Ord)
                
-- Data que guarda os comandos para nossa lingugagem
data Commands = Nop								  | Atrib Char AritExp                |
                Seq Commands Commands             | Choice BoolExp Commands  		  | -- por que 3 comandos?
                Loop BoolExp Commands deriving (Show)

memory = initial

sto2 = update memory 'x' 4

evalBoolExp::BoolExp -> Store -> Bool
evalBoolExp (B val) _ 				= val -- testado
evalBoolExp (No exp1) store 		=  not (evalBoolExp exp1 store) -- testado
evalBoolExp (And' exp1 exp2) store 	= (evalBoolExp exp1 store) && (evalBoolExp exp2 store) -- testado
evalBoolExp (Or' exp1 exp2) store 	= (evalBoolExp exp1 store) || (evalBoolExp exp2 store) -- testado
evalBoolExp (Great exp1 exp2) store = (evalAritExp exp1 store) > (evalAritExp exp2 store) -- testado
evalBoolExp (Less exp1 exp2) store 	= (evalAritExp exp1 store) < (evalAritExp exp2 store) -- testado
evalBoolExp (Equal exp1 exp2) store = ( evalAritExp exp1 store) == (evalAritExp exp2 store) -- testado

evalAritExp:: AritExp-> Store -> Integer
evalAritExp (L val) _ 				= val -- testado
evalAritExp (V c) store 			= value store c -- testado
evalAritExp (Sub exp1 exp2) store 	= (evalAritExp exp1 store) - (evalAritExp exp2 store) -- testado
evalAritExp (Add exp1 exp2) store 	= (evalAritExp exp1 store) + (evalAritExp exp2 store) -- testado
evalAritExp (Mult exp1 exp2) store 	= (evalAritExp exp1 store) * (evalAritExp exp2 store) -- testado
evalAritExp (Div exp1 exp2) store 	= (evalAritExp exp1 store) `div` (evalAritExp exp2 store) -- testado

evalCommands::Commands -> Store -> Store
evalCommands (Nop) store = store
evalCommands (Atrib name val) store = update store name (evalAritExp val store)
evalCommands (Seq comd1 comd2) store = joinStore(evalCommands comd1 store) (evalCommands comd2 store) 


teste = evalAritExp (Mult (V 'x') (Div (L 20) (L 10)))  sto2

-- Inicio de um programa
atrib00 = Atrib 'f' (L 1)

atrib00Loop = Great (V 'n') (L 0)
atrib01Loop = Atrib 'f' (Mult(V 'f') (V 'n'))
atrib02Loop = Atrib 'n' (Sub(V 'n') (L 1))
loop = Loop atrib00Loop (Seq atrib01Loop atrib02Loop)

prog = Seq atrib00 loop         

