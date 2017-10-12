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
                Seq Commands Commands             | Choice BoolExp Commands Commands  | -- Choice seria um If then else
                Loop1 BoolExp Commands 			  |	Loop2  Commands BoolExp	deriving (Show)

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
evalCommands (Seq comd1 comd2) store =  (evalCommands comd2 (evalCommands comd1 store)) -- comd2 utiliza o store do comd1
evalCommands (Choice expbool comd1 comd2) store = evalCommands (funcChoice expbool comd1 comd2 store) store
evalCommands ( Loop1 expbool comd) store = funcLoop expbool comd store
evalCommands ( Loop2 comd expbool) store = funcLoop expbool comd store

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

teste = evalAritExp (Mult (V 'x') (Div (L 20) (L 10)))  sto2

-- Inicio de um programa
--atrib00 = Atrib 'f' (L 1)

atrib00Loop = Less (V 'n') (L 5)
atrib01Loop = Atrib 'n' (Add(V 'n') (L 1))
atrib02Loop = Atrib 'f' (Mult(V 'f') (L 2))
atrib03Loop = Atrib 'f' (Add(V 'f') (L 1))
loop = Loop2 (Seq (Seq atrib01Loop atrib02Loop) atrib03Loop) atrib00Loop

--prog = Seq atrib00 loop

-- Inicio programa 2
atrib00 = Atrib 'f' (L 4)
atrib00If = Equal (V 'f') (L 5)
atrib01If = Atrib 'f' (Mult (V 'f') (L 2))
atrib01 = Atrib 'n' (L 12)
seqif = Choice atrib00If atrib01If atrib01
prog = Seq atrib00 seqif