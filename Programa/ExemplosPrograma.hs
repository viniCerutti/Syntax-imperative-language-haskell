module ExemplosPrograma where

import T1ProgFuncional
import Store

{-
	Programa 01: Calculo do fatorial com apenas somas

	Em outra linguagem de programacao

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

memoryFat = initial

-- atribuicao inicial de n
fatAtribN = Atrib 'n' (L 4) -- numero que se deseja descobrir o fatorial
fatAtribM = Atrib 'm' (L 0) -- variavel auxiliar para realizar a somas sucessivas
fatAtribR = Atrib 'r' (L 1) -- variavel para guardar o resultado
fatBoolLoop1 = Great (V 'n') (L 0) -- expressao n > 0 do primeiro laco
fatAtrib1Loop1 = Atrib 'm' (V 'n') -- m = n
fatAtrib2Loop1 = Atrib 't' (L 0) -- t = 0
fatBoolLoop2 = Great (V 'm') (L 0) -- expressao m > 0 do segundo laco
fatAtrib1Loop2 = Atrib 't' (Add (V 't') (V 'r')) -- t = t + r
fatAtrib2Loop2 = Atrib 'm' (Sub (V 'm') (L 1)) -- m = m - 1
fatAtrib3Loop1 = Atrib 'r' (V 't') -- r = t
fatAtrib4Loop1 = Atrib 'n' (Sub (V 'n') (L 1)) -- n = n - 1

fatLoop2 = Loop1 fatBoolLoop2 (Seq fatAtrib1Loop2 fatAtrib2Loop2)
fatLoop1 = Loop1 fatBoolLoop1 (Seq fatAtrib1Loop1 (Seq fatAtrib2Loop1 (Seq fatLoop2 (Seq fatAtrib3Loop1 fatAtrib4Loop1))))

fatorial = Seq fatAtribN (Seq fatAtribM (Seq fatAtribR fatLoop1))

resultFat = value (evalCommands fatorial memoryFat) 'r'

{-
	Programa 02: Calculo daa multiplicacao com apenas soma

	Em outra lingugem de programacao

	  int n, m; // multiplicando e multiplicador da multiplicacao
	  int r = 0;
	 do{
	     r = r+m;
	     n = n - 1;
	 }while(n > 0);

-}

memoryMult = initial

multAtribN = Atrib 'n' (L 4) -- numero multiplicando
multAtribM = Atrib 'm' (L 4) -- numero multiplicador
multAtribR = Atrib 'r' (L 0) -- variavel para guardar o resultado
multBoolLoop = Great (V 'n') (L 0) -- expressao n > 0 do loop
multAtrib1Loop = Atrib 'r' (Add (V 'r') (V 'm')) -- r = r + m
multAtrib2Loop = Atrib 'n' (Sub (V 'n') (L 1)) -- n = n - 1

multLoop = Loop2 (Seq multAtrib1Loop multAtrib2Loop) multBoolLoop

multiplicao = Seq multAtribN (Seq multAtribM (Seq multAtribR multLoop))

resultMult = value (evalCommands multiplicao memoryMult) 'r'
