module ExemplosPrograma where

import T1ProgFuncional
import Store



{-
	Programa 01: Calculo do fatorial com apenas somas
	em outra linguagem de programacao

	  int n, m; // n é o numero para se calcular o fatorial
	  int r = 1;

	  while (n > 0) {
	    
	    m = n;
	    int t = 0;
	    while(m > 0){
	        t = t + r;
	        m = m - 1;
	    }
	    r = t;
	    n = n - 1;
	  }

-}

memoryProg1 = initial

-- atribuicao inicial de n
prog1AtribN = Atrib 'n' (L 4) -- numero que se deseja descobrir o fatorial
prog1AtribM = Atrib 'm' (L 0) -- variavel auxiliar para realizar a somas sucessivas
prog1AtribR = Atrib 'r' (L 1) -- variavel para guardar o resultado
prog1BoolLoop1 = Great (V 'n') (L 0) -- expressao n > 0 do primeiro laco
prog1Atrib1Loop1 = Atrib 'm' (V 'n') -- m = n
prog1Atrib2Loop1 = Atrib 't' (L 0) -- t = 0
prog1BoolLoop2 = Great (V 'm') (L 0) -- expressao m > 0 do segundo laco
prog1Atrib1Loop2 = Atrib 't' (Add (V 't') (V 'r')) -- t = t + r
prog1Atrib2Loop2 = Atrib 'm' (Sub (V 'm') (L 1)) -- m = m - 1
prog1Atrib3Loop1 = Atrib 'r' (V 't') -- r = t
prog1Atrib4Loop1 = Atrib 'n' (Sub (V 'n') (L 1)) -- n = n - 1

pro1Loop2 = Loop1 prog1BoolLoop2 (Seq prog1Atrib1Loop2 prog1Atrib2Loop2)
pro1Loop1 = Loop1 prog1BoolLoop1 (Seq prog1Atrib1Loop1 (Seq prog1Atrib2Loop1 (Seq pro1Loop2 (Seq prog1Atrib3Loop1 prog1Atrib4Loop1))))

programa1 = Seq prog1AtribN (Seq prog1AtribM (Seq prog1AtribR pro1Loop1))

resultProg1 = value (evalCommands programa1 memoryProg1) 'r'