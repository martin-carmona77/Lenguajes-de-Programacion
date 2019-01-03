{-Facultad de Ciencias UNAM - Lenguajes de programación 2019-1 
      Vázquez Reyes C. Moisés
      Rafael de Jesús García García 
      Magallanes Ramírez Diego Alberto
-}

{-MÁQUINA K-}

--Expresiones aritméticas-booleanas con funciones y continuaciones.   
data LamAB =  Var String |
              VNum Int   |
              VBool Bool |
              Neg LamAB |
              Suma LamAB LamAB | 
              Ifte LamAB LamAB LamAB |
              Let String LamAB LamAB |
              Menor LamAB LamAB |
              Lambda String LamAB |
              App LamAB LamAB |
              Cont Pila |                              --Pila materializada
              Letcc String LamAB |                     --Operador letcc
              Continue LamAB LamAB deriving (Show,Eq)  --Continue e1 e2


--Marcos de operación
data Marco = MSumI () LamAB  | --Marco suma izq
             MSumD LamAB ()  deriving (Show,Eq) --Marco suma derecha

--Pila de control
type Pila = [Marco]

--Estados de la Máquina K
data EstadoMK = Ev (Pila,LamAB)   --corresponde a P≻e 
              | Dv (Pila,LamAB)   --corresponde a P≺v
                      deriving Show    


--SEMÁNTICA DINÁMICA


--Realiza un paso de evaluación en la máquina K
eval1 :: EstadoMK->EstadoMK
eval1 = error "Te toca"                           


--Realiza una ejecución completa en la máquina K
evalK :: EstadoMK->EstadoMK
evalK = error "Te toca"








