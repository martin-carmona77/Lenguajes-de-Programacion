{-Facultad de Ciencias UNAM - Lenguajes de programación 2019-1 
      Araujo Chávez Mauricio
      Carmona Mendoza Martín
-}

module AnalSintc where

{----------------------}
--ANÁLISIS SINTÁCTICO--
{----------------------}

--Juicio 'Pila'.
data JuicioPila = Pila (Int,String) deriving Show

--Parser para el lenguaje de paréntesis balanceados.
parser::JuicioPila->Bool
parser (Pila (0,"")) = True
parser (Pila (_,"")) = False
parser (Pila (n, ('(':xs))) = parser (Pila(n+1,xs))
parser (Pila (n, (')':xs))) = parser (Pila(n-1,xs))


{-----------------------------}
--EVALUADOR DE EXPRESIONES EA--
{-----------------------------}

--Gramática sin ambigüedad para expresiones aritméticas.
data E = ExpT T | ExpSum E T
data T = TermF F | ProdT E F
data F = NumF N | ParF E
data N = Dig D | Num N D
data D = Cero | Uno | Dos | Tres | Cuatro | Cinco | Seis | Siete | Ocho | Nueve

evalD::D->Int
evalD Cero = 0
evalD Uno = 1
evalD Dos = 2
evalD Tres = 3
evalD Cuatro = 4
evalD Cinco = 5
evalD Seis = 6
evalD Siete = 7
evalD Ocho = 8
evalD Nueve = 9

evalN::N->Int
evalN (Dig d) = evalD d
evalN (Num n d) = (evalN n * 10) + evalD d

evalF::F->Int
evalF (NumF n) = evalN n
evalF (ParF p) = evalE p

evalT::T->Int
evalT (TermF f) = evalF f
evalT (ProdT e f) = evalE e * evalF f

evalE::E->Int
evalE (ExpT t) = evalT t
evalE (ExpSum e t) = evalE e + evalT t

--Evaluador de expresiones aritméticas.
eval::E->Int
eval e = evalE e  
          
{-Codifica las siguientes expresiones-}
-- 4+(5*3)      
e1 = eval (ExpSum (ExpT (TermF (NumF (Dig Cuatro)))) (ProdT (ExpT (TermF (NumF (Dig Cinco)))) (NumF (Dig Tres))))

--(4+5)*3
e2 = eval (ExpT (ProdT (ExpSum (ExpT (TermF (NumF (Dig Cuatro)))) (TermF (NumF (Dig Cinco)))) (NumF (Dig Tres))))

--(3+(5+7))*(4*3)
e3 = eval (ExpT (ProdT (ExpSum (ExpT (TermF (NumF (Dig Tres)))) (TermF (ParF (ExpSum (ExpT (TermF (NumF (Dig Cinco)))) (TermF (NumF (Dig Siete))))))) (ParF (ExpT (ProdT (ExpT (TermF (NumF (Dig Cuatro)))) (NumF (Dig Tres)))))))

-- Prueba
p1 = parser(Pila(0,"(())"))