{-
    T1 - Programacao Funcional 2017/2
    Vinicius Cerutti
    Dimas Olympio
-}

module InterpLingImpHaskell where

import Store
import Data.List

 -- O tipo de dado que sera a variavel 
 -- (isso depende da implementacao do store)
type Var = Char

-- o tipo do dado que sera armazenado dentro da variavel
 -- (isso depende da implementacao do store)
type ValueVar = Integer

-- Data que guarda as operacoes aritmeticas

data AritExp =  L ValueVar                | Sub AritExp AritExp |
                Add AritExp AritExp      | Mult AritExp AritExp|
                Div AritExp AritExp      | Mod' AritExp AritExp|
                Abs' AritExp             | V Var  deriving (Show,Eq,Ord)
            
-- Data que guarda as operacoes logicas

data BoolExp =  B Bool                | No BoolExp          |
                And' BoolExp BoolExp  | Or' BoolExp BoolExp |
                Great AritExp AritExp | Less AritExp AritExp|
                Equal AritExp AritExp  deriving (Show,Eq,Ord)
                
-- Data que guarda os comandos para nossa lingugagem

data Commands = Nop                          | Atrib Var AritExp                |
                Seq Commands Commands        | Choice BoolExp Commands Commands  | -- Choice seria um If then else
                While BoolExp Commands       | Dowhile  Commands BoolExp    deriving (Show)

-- Fucao que avalia (evaluate) as expressoes aritmeticas

evalAritExp:: AritExp-> Store -> ValueVar
evalAritExp (L val) _ = val -- testado
evalAritExp (V c) store = value store c -- testado
evalAritExp (Sub exp1 exp2) store = (evalAritExp exp1 store) - (evalAritExp exp2 store) -- testado
evalAritExp (Add exp1 exp2) store = (evalAritExp exp1 store) + (evalAritExp exp2 store) -- testado
evalAritExp (Mult exp1 exp2) store = (evalAritExp exp1 store) * (evalAritExp exp2 store) -- testado
evalAritExp (Div exp1 exp2) store  = (evalAritExp exp1 store) `div` (evalAritExp exp2 store) -- testado
evalAritExp (Mod' exp1 exp2) store = (evalAritExp exp1 store) `mod` (evalAritExp exp2 store) -- testado
evalAritExp (Abs' exp1) store = abs (evalAritExp exp1 store) -- testado

-- Fucao que avalia (evaluate) as expressoes logicas

evalBoolExp::BoolExp -> Store -> Bool
evalBoolExp (B val) _ = val -- testado
evalBoolExp (No exp1) store =  not (evalBoolExp exp1 store) -- testado
evalBoolExp (And' exp1 exp2) store = (evalBoolExp exp1 store) && (evalBoolExp exp2 store) -- testado
evalBoolExp (Or' exp1 exp2) store = (evalBoolExp exp1 store) || (evalBoolExp exp2 store) -- testado
evalBoolExp (Great exp1 exp2) store = (evalAritExp exp1 store) > (evalAritExp exp2 store) -- testado
evalBoolExp (Less exp1 exp2) store = (evalAritExp exp1 store) < (evalAritExp exp2 store) -- testado
evalBoolExp (Equal exp1 exp2) store = ( evalAritExp exp1 store) == (evalAritExp exp2 store) -- testado

-- Fucao que avalia (evaluate) a sintaxe do programa

evalCommands::Commands -> Store -> Store
evalCommands (Nop) store = store
evalCommands (Atrib name val) store = update store name (evalAritExp val store)
evalCommands (Seq comd1 comd2) store =  (evalCommands comd2 (evalCommands comd1 store)) -- comd2 utiliza o store do comd1
evalCommands (Choice expbool comd1 comd2) store = evalCommands (funcChoice expbool comd1 comd2 store) store -- if...then...else
evalCommands ( While expbool comd) store = funcLoop expbool comd store -- while
evalCommands ( Dowhile comd expbool) store = funcLoop expbool comd store -- do...while

-- Funcao auxiliar para avaliacao do tipo Choice(selecao)
-- se a expressao logica for veradeira retorna o primeiro comando
-- senão retorna o segundo comando

funcChoice :: BoolExp -> Commands -> Commands -> Store -> Commands
funcChoice expBool comd1 comd2 store
    | evalBoolExp expBool store = comd1
    | otherwise = comd2

-- Funcao auxiliar para avalicao do tipo While ou DoWhile
-- que atualiza a memoria ate a condicao logica for falsa

funcLoop :: BoolExp -> Commands -> Store -> Store
funcLoop expBool comd store
    | evalBoolExp expBool store =  funcLoop expBool comd command 
    | otherwise = store
        where
            command = evalCommands comd store

-- Funcao para imprimir os valores das variaveis que foram
-- executados durante do programa, onde a impressao informa
-- o valor e a variavel antes e depois da execucao do programa

pretty_printing :: Commands -> Store -> IO()
pretty_printing prog store = putStrLn (formatText memIniVariables memFinVariavles)
        where
            progVariables = getVars prog
            memProg = evalCommands prog store
            memFinVariavles = getVarMem progVariables memProg 
            memIniVariables = getVarMem progVariables store 

            -- Funcao que formata o texto para impressao
            -- onde primeiro se imprime a memoria antes de executar 
            -- o programa e depois separada por traços (---) a impressao
            -- da memoria depois da execucao do programa

            formatText:: [(ValueVar,Var)] -> [(ValueVar,Var)] -> String
            formatText storeI storeF = traceStr++"\nStoreIni = "++storeIni++"\n"++traceStr++"\nStoreFin = "++storeFin++"\n"++traceStr
                where
                    storeIni = show storeI
                    storeFin = show storeF
                    traceNumber = max (length storeIni) (length storeFin)
                    traceStr = copyChar '-' traceNumber

                    -- Funcao que copia o caractere informando n vezes
                    copyChar:: Char -> Int -> String
                    copyChar c num
                        | num <=0 = ""
                        |otherwise = copyChar c (num-1)++[c]

-- Funcao que retorna uma lista das variaveis 
-- utilizadas no programa
getVars:: Commands -> [Var]
getVars comd =  (nub . getVarsAux) comd
    where
        getVarsAux (Nop) = []
        getVarsAux (Atrib name val) = [name]
        getVarsAux (Seq comd1 comd2) =  (getVarsAux comd2)++(getVarsAux comd1)
        getVarsAux (Choice _ comd1 comd2) = (getVarsAux comd1)++(getVarsAux comd2)
        getVarsAux ( While _ comd) = getVarsAux comd
        getVarsAux ( Dowhile comd _ ) = getVarsAux comd

-- Funcao que retorna apenas uma lista da memoria
-- com os valores das variaveis informadas

getVarMem::[Var] -> Store -> [(ValueVar,Var)]
getVarMem [] _ = []
getVarMem (name:xs) store = (number,name):getVarMem xs store
    where
        number = value store name