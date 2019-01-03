{-Facultad de Ciencias UNAM - Lenguajes de programación 2019-1 
      Vázquez Reyes C. Moisés
      Rafael de Jesús García García 
      Magallanes Ramírez Diego Alberto
-}

{-MÁQUINA K-}

--Expresiones aritméticas-booleanas con funciones.   
data LamAB =  Var String |
              VNum Int   |
              VBool Bool |
              Neg LamAB |
              Suma LamAB LamAB | 
              Ifte LamAB LamAB LamAB |
              Let String LamAB LamAB |
              Menor LamAB LamAB |
              Lambda String LamAB |
              App LamAB LamAB deriving (Show,Eq)


--Marcos de operación
data Marco = MSumI () LamAB            | --Marco suma izq
             MSumD LamAB ()            | --Marco suma derecha 
             MNeg ()                   | --Marco negacion 
             MIf () LamAB LamAB        | --Marco ifte
             MLet String () LamAB      | --Marco Let
             MMenI () LamAB            | --Marco menor izq
             MMenD LamAB ()            | --Marco menor derecha
             MAppI () LamAB            | --Marco aplicacion izq
             MAppD LamAB () deriving (Show,Eq) --Marco aplicacion derecha
             

--Pila de control
type Pila = [Marco]

--Estados de la Máquina K
data EstadoMK = Ev (Pila,LamAB)   --corresponde a P≻e 
              | Dv (Pila,LamAB)   --corresponde a P≺v
                      deriving Show    


--SEMÁNTICA DINÁMICA

sust :: LamAB -> String -> LamAB -> LamAB
sust e s r = case e of
               Var v -> if (v == s)
                        then r
                        else e
               VNum n -> VNum n
               VBool b -> VBool b
               Suma e1 e2 -> Suma (sust e1 s r) (sust e2 s r)
               Ifte e1 e2 e3 -> Ifte (sust e1 s r) (sust e2 s r) (sust e3 s r)
               l@(Let s1 e1 e2) -> if (elem s1 (freeVars r ++ [s]))
                                   then l
                                   else (Let s1 (sust e1 s r) (sust e2 s r))
               Menor e1 e2 -> Menor (sust e1 s r) (sust e2 s r)
               Neg e -> Neg (sust e s r)
               App e1 e2 -> App (sust e1 s r) (sust e2 s r)
               l@(Lambda a e) -> if (elem a (freeVars r ++ [s]))
                                    then l
                                    else (Lambda a (sust e s r))
               

freeVars :: LamAB->[String]
freeVars e = case e of
               Var v -> [v]
               VNum n -> []
               VBool b -> []
               Suma e1 e2 -> freeVars e1 ++ freeVars e2
               Ifte e1 e2 e3 -> freeVars e1 ++ freeVars e2 ++ freeVars e3
               Let x e1 e2 -> filter (/=x) (freeVars e1 ++ freeVars e2)
               Menor e1 e2 -> freeVars e1 ++ freeVars e2
               Neg e -> freeVars e
               App e1 e2 -> freeVars e1 ++ freeVars e2
               Lambda s e -> filter (/=s) (freeVars e)
               

--Realiza un paso de evaluación en la máquina K
eval1 :: EstadoMK->EstadoMK
eval1 e = case e of
            Ev (p,Var v) -> Dv (p,Var v)

            Ev (p,VNum n) -> Dv (p,VNum n)

            Ev (p,VBool b) -> Dv (p,VBool b)

            Ev (p,(Lambda s e)) -> Dv (p,(Lambda s e))

            Ev (p,Suma e1 e2) -> Ev ((MSumI () e2):p,e1)
            Dv ((MSumI () e2):p,VNum n) -> Ev ((MSumD (VNum n) ()):p,e2)
            Dv ((MSumD (VNum n) ()):p,VNum m) -> Dv (p, VNum (n+m))

            Ev (p,Ifte e1 e2 e3) -> Ev ((MIf () e2 e3):p,e1)
            Dv ((MIf () e2 e3):p,VBool True) -> Ev (p,e2)
            Dv ((MIf () e2 e3):p,VBool False) -> Ev (p,e3)
       
            Ev (p,Let n e1 e2) -> Ev ((MLet n () e2):p,e1)
            Dv ((MLet n () e2):p,v) -> Ev (p,sust e2 n v)
            
            Ev (p,Menor e1 e2) -> Ev ((MMenI () e2):p,e1)
            Dv ((MMenI () e2):p,VNum n) -> Ev ((MMenD (VNum n) ()):p,e2)
            Dv ((MMenD (VNum n) ()):p,VNum m) -> Dv (p, VBool (n < m))
            
            Ev (p,Neg e1) -> Ev ((MNeg ()):p,e1)
            Dv ((MNeg ()):p,VBool b) -> Dv (p,VBool (not b))
            
            Ev (p,App e1 e2) -> Ev ((MAppI () e2:)p,e1)
            Dv ((MAppI () e2):p,v) -> Ev ((MAppD v ()):p,e2)
            Dv ((MAppD (Lambda s e3) ()):p,v) -> Ev (p,sust e3 s v)

            
esFinal :: EstadoMK->Bool
esFinal e = case e of
              Dv ([],Var v) -> True
              Dv ([],VNum n) -> True
              Dv ([],VBool b) -> True
              _ -> False

--Realiza una ejecución completa en la máquina K
evalK :: EstadoMK->EstadoMK
evalK l = if(esFinal l)
          then l
          else evalK(eval1 l)


--Pruebas
prueba0 = evalK $ Ev ([], Suma (Suma (VNum 1) (VNum 2)) (VNum 3))
prueba1 = evalK $ Ev ([], Ifte (Neg (VBool False)) (Suma (VNum 5) (VNum 7)) (VNum 77))
prueba2 = evalK $ Ev ([], Ifte (Menor (Suma (VNum 5) (VNum 8))(VNum 13)) ((VNum 1)) (VNum 2))
prueba3 = evalK $ Ev ([], Ifte (Menor (Suma (VNum 5) (VNum 8))(VNum 14)) (Let "x" (VNum 8) (Suma (Var "x") (Var "x")))
                          (Let "x" (VNum 3) (Suma (Var "x") (Var "x"))))
prueba4 = evalK $ Ev ([], Ifte (Neg (VBool True)) (Let "x" (VNum 8) (Suma (Var "x") (Var "x")))
                          (Let "x" (VNum 3) (Suma (Var "x") (Var "x"))))
prueba5 = evalK $ Ev ([],App (Lambda ("x")(Ifte (Neg (VBool False)) (App (Lambda ("y")(Var "y")) (VNum 14))
                                           (App (Lambda ("z")(Suma (Var "z") (VNum 1))) (VNum 3))))(VNum 2))
