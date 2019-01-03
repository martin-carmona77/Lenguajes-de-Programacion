{-Facultad de Ciencias UNAM - Lenguajes de programación 2019-1 
      Vázquez Reyes C. Moisés
      Rafael de Jesús García García 
      Magallanes Ramírez Diego Alberto
-}

-- Araujo Chavez Mauricio 312210047
-- Carmona Mendoza Martin 313075977

data EAB = Var String
         | VNum Int
         | VBool Bool
         | Suma EAB EAB
         | Prod EAB EAB
         | Ifte EAB EAB EAB
         | Iszero EAB
         | Let String EAB EAB
         | Menor EAB EAB
         | Eq EAB EAB
         | Neg EAB
         | L Int 
         | Asig EAB EAB 
         | Ref EAB 
         | Deref EAB 
         | Seq EAB EAB 
         | While EAB EAB 
         | Unit deriving (Show,Eq)

-- Una LDir es una dirección de memoria.
-- Unicamente usaremos el caso 'L Int' del tipo EAB.
type LDir = EAB

--Usamos este alias para enfatizar que una memoria guarda valores.
type Val = EAB

--Una memoria es una lista de tuplas donde la primer entrada de cada tupla
--es una dirección de memoria y la segunda es un valor.
type Mem = [(LDir,Val)] 


{-EJERCICIOS:-}
{-Semántica dinámica-}

sacaDirs :: Mem -> [LDir]
sacaDirs [] = []
sacaDirs (x:xs) = [fst x] ++ (sacaDirs xs)               

sacaVal :: LDir -> Mem -> Val
sacaVal (L x) ((L y, v):xs) = if x == y
                              then v
                              else sacaVal (L x) (xs)  


esValor :: EAB -> Bool
esValor e = case e of
              VNum _ -> True
              VBool _ -> True
              L _ -> True
              Unit -> True
              _ -> False
  
accessMem :: LDir -> Mem -> Maybe Val
accessMem l m = if elem l (sacaDirs m) then Just $ sacaVal l m else Nothing 

update :: LDir -> Val -> Mem -> Mem
update l v m = if elem l (sacaDirs m)
               then [if (fst x) == l then (l,v) else x |x <- m]
               else m ++ [(l,v)]

eval1 :: (Mem,EAB)->(Mem,EAB)
eval1 (m,e) = case e of
                VNum  _ -> (m,e)
                VBool _ -> (m,e)
                Suma (VNum x) (VNum y) -> (m, VNum $ x + y)
                Suma e1@(VNum n) e2 -> let (m',e2') = eval1 (m,e2) in (m', Suma e1 e2')
                Suma e1 e2 -> let (m',e1') = eval1 (m,e1) in (m', Suma e1' e2)
                Prod (VNum x) (VNum y) -> (m, VNum $ x * y)
                Prod e1@(VNum n) e2 -> let (m',e2') = eval1 (m,e2) in (m', Prod e1 e2')
                Prod e1 e2 -> let (m',e1') = eval1 (m,e1) in (m', Prod e1' e2)
                Ifte (VBool True) e2 e3 -> (m,e2)
                Ifte (VBool False) e2 e3 -> (m,e3)
                Ifte e1 e2 e3 -> let (m',e1') = eval1 (m,e1) in (m', Ifte e1' e2 e3)
                Iszero (VNum x) -> (m, VBool $ x == 0)
                Iszero e -> let (m',e') = eval1 (m,e) in (m', Iszero e')
                Menor (VNum x) (VNum y) -> (m, VBool $ x < y)
                Menor e1@(VNum n) e2 -> let (m',e2') = eval1 (m,e2) in (m', Menor e1 e2')
                Menor e1 e2 -> let (m',e1') = eval1 (m,e1) in (m', Menor e1' e2)
                Eq (VNum x) (VNum y) -> (m, VBool $ x == y)
                Eq e1@(VNum n) e2 -> let (m',e2') = eval1 (m,e2) in (m', Eq e1 e2')
                Eq e1 e2 -> let (m',e1') = eval1 (m,e1) in (m', Eq e1' e2)
                Neg (VBool True) -> (m, VBool False)
                Neg (VBool False) -> (m, VBool True)
                Neg e -> let (m',e') = eval1 (m,e) in (m', Neg e')
                L x -> (m,e)
                Unit -> (m,Unit)
                Seq Unit e2 -> (m,e2)
                Seq e1 e2 -> let (m',e1') = eval1 (m,e1) in (m', Seq e1' e2)
                w@(While e1 e2) -> (m, Ifte e1 (Seq e2 w) Unit) 
                Asig l@(L n) e2 -> if esValor e2
                                   then (update l e2 m, Unit)
                                   else let (m',e2') = eval1 (m,e2) in (m', Asig l e2')
                Asig e1 e2 -> let (m',e1') = eval1 (m,e1) in (m', Asig e1' e2)
                Deref l@(L n) -> if existeL
                                 then (m,sacaVal l m)
                                 else error "Direccion no encontrada" where existeL = elem l (sacaDirs m)
                Deref e -> let (m',e') = eval1 (m,e) in (m', Deref e')
                Ref e -> if (esValor e)
                         then ((L (nuevaDir m),e):m, L (nuevaDir m))
                         else let (m',e') = eval1 (m,e) in (m', Ref e') 
                Let x e1 e2 -> if esValor e1
                               then (m, sust e2 x e1)
                               else let (m',e1') = eval1 (m,e1) in (m', Let x e1' e2) 

sust :: EAB -> String -> EAB -> EAB
sust e s r = case e of
               Var v -> if (v == s)
                        then r
                        else e
               VNum n -> VNum n
               VBool b -> VBool b
               Suma e1 e2 -> Suma (sust e1 s r) (sust e2 s r)
               Prod e1 e2 -> Prod (sust e1 s r) (sust e2 s r)
               Ifte e1 e2 e3 -> Ifte (sust e1 s r) (sust e2 s r) (sust e3 s r)
               Iszero e -> Iszero $ sust e s r
               Menor e1 e2 -> Menor (sust e1 s r) (sust e2 s r)
               Eq e1 e2 -> Eq (sust e1 s r) (sust e2 s r)
               Neg e -> Neg (sust e s r)
               Asig e1 e2 -> Asig (sust e1 s r) (sust e2 s r)
               Ref e -> Ref (sust e s r)
               Deref e -> Deref (sust e s r)
               L n -> L n
               Seq e1 e2 -> Seq (sust e1 s r) (sust e2 s r)
               While e1 e2 -> While (sust e1 s r) (sust e2 s r)
               Unit -> Unit
               l@(Let s1 e1 e2) -> if (elem s1 (freeVars r ++ [s]))
                                   then l
                                   else (Let s1 (sust e1 s r) (sust e2 s r))
           

freeVars :: EAB -> [String]
freeVars e = case e of
               Var v -> [v]
               VNum n -> []
               VBool b -> []
               Suma e1 e2 -> freeVars e1 ++ freeVars e2
               Prod e1 e2 -> freeVars e1 ++  freeVars e2
               Ifte e1 e2 e3 -> freeVars e1 ++ freeVars e2 ++ freeVars e3
               Iszero e -> freeVars e
               Menor e1 e2 -> freeVars e1 ++ freeVars e2
               Eq e1 e2 -> freeVars e1 ++ freeVars e2
               Neg e -> freeVars e
               Asig e1 e2 -> freeVars e1 ++ freeVars e2
               Ref e -> freeVars e
               Deref e -> freeVars e
               L n -> []
               Seq e1 e2 -> freeVars e1 ++ freeVars e2
               While e1 e2 -> freeVars e1 ++ freeVars e2
               Unit -> []
               Let x e1 e2 -> filter (/=x) (freeVars e1 ++ freeVars e2)

nuevaDir :: Mem -> Int
nuevaDir m = aux1 m 0

aux1 :: Mem -> Int -> Int
aux1 m i = if (elem (L i) [fst x | x <- m])
             then aux1 m (i + 1)
             else i
                  
evals :: (Mem,EAB)->(Mem,EAB)
evals (m,e) = if esValor e then (m,e) else evals $ eval1 (m,e) 

interp :: EAB->EAB
interp eab = snd $ evals ([] , eab)                     


{-Aquí van tus cinco pruebas para la semántica dinámica-}

prueba1 = interp $ Let "x" (Ref $ VNum 3) (Suma (VNum 4) (Deref $ Var "x"))
prueba2 = interp $ Let "x" (Ref $ VNum 2938) (Suma (VNum 123) (Deref $ Var "x"))
prueba3 = interp $ Ifte (VBool True) (Suma (VNum 7) (VNum 8)) (Suma (VNum 4) (VNum 3))
prueba4 = interp $ Ifte (VBool False) (Suma (VNum 7) (VNum 8)) (Suma (VNum 4) (VNum 3))
prueba5 = interp $ Ifte (Menor (VNum 4) (VNum 7)) (Neg (VBool True)) (Neg (VBool False))
prueba6 = interp $ Ifte (Menor (VNum 10) (VNum 7)) (Neg (VBool True)) (Neg (VBool False))
prueba7 = interp $ Ifte (Iszero (VNum 7)) (Suma (VNum 7) (VNum 7)) (Prod (VNum 7) (VNum 7)) 

isPair :: Int -> EAB
isPair n = interp $ Let "b" (Ref $ VBool True) $ Let "n" (Ref $ VNum n) $ Seq
           (While (Neg $ Iszero (Deref $ Var "n")) $
             Seq (Asig (Var "b") (Neg $ Deref $ Var "b"))
             (Asig (Var "n") (Suma (Deref $ Var "n") (VNum $ -1))))
           $ Ifte (Deref $ Var "b") (VBool True) (VBool False) 

factorial :: Int -> EAB
factorial n = interp $ Let "x" (Ref $ VNum n) $ Let "y" (Ref $ VNum 1)
              $ Seq (While (Menor (VNum 0) (Deref $ Var "x"))
                      $ Seq (Asig (Var "y") (Prod (Deref $ Var "x") (Deref $ Var "y")))
                      (Asig (Var "x") (Suma (Deref $ Var "x") (VNum $ -1))))
              $ Deref $ Var "y"

{-Semántica estática-}


data Tipo = TInt | TBool | TUnit | TRef Tipo deriving (Show,Eq)



--Los contextos ahora incluyen un conjunto exclusivo para direcciones de memoria.
type Ctx = ([(String,Tipo)],[(LDir,Tipo)])


vt :: Ctx->EAB->Tipo
vt ctx e = case e of
             VNum _ -> TInt
             VBool _ -> TBool
             Suma e1 e2 -> if vt ctx e1 == TInt
                           then if vt ctx e2 == TInt
                                then TInt
                                else error "La segunda expresión de Suma no es TInt."
                           else error "La primera expresión de Suma no es TInt."
             Prod e1 e2 -> if vt ctx e1 == TInt
                           then if vt ctx e2 == TInt
                                then TInt
                                else error "La segunda expresión de Suma no es TInt."
                           else error "La primera expresión de Suma no es TInt."
             Ifte e1 e2 e3 -> if vt ctx e1 == TBool
                              then let t2 = vt ctx e2 in let t3 = vt ctx e3 in
                                                           if t2 == t3
                                                           then t2
                                                           else error "El tipo de las ramas de Ifte son diferentes"
                              else error "La bandera del Ifte no es de tipo TBool"
             Iszero e -> let t = vt ctx e in if (t == TInt)
                                             then TBool
                                             else error $ "La expresión no es de tipo TInt"
             Let x e1 e2 -> vt ctx' e2 where ctx' = ((x, vt ctx e1):fst ctx, snd ctx)
             Menor e1 e2 -> let
               t1 = vt ctx e1
               t2 = vt ctx e2 in if (t1 == TInt && t2 == TInt)
                                 then TBool
                                 else (if (t1 /= TInt)
                                       then error $ "La primera expresión no es de tipo TInt"
                                       else error $ "La segunda expresion no es de tipo TInt")
             Eq e1 e2 -> let
               t1 = vt ctx e1
               t2 = vt ctx e2 in if (t1 == TInt && t2 == TInt)
                                 then TBool
                                 else (if (t1 /= TInt)
                                       then error $ "La primera expresión no es de tipo TInt"
                                       else error $ "La segunda expresión no es de tipo TInt")

             Neg e -> let
               t = vt ctx e in if (t == TBool)
                               then TBool
                               else error $ "La expresión no es de tipo TBool"
             L n -> case ctx of
                      (ctxV,[]) -> error $ "La memoria es vacia"
                      (ctxV,(L m,l):ctxL) -> if  (n == m)
                                             then TRef l
                                             else vt (ctxV,ctxL) e 
             Asig e1 e2 -> let
               TRef t1 = vt ctx e1
               t2 = vt ctx e2 in if (t2 == t1)
                                 then TUnit
                                 else error $ "La expresion no es de tipo TRef" 
             Ref e -> TRef (vt ctx e)
             Deref e -> if (vt ctx e) == TRef TInt
                        then TInt
                        else if (vt ctx e) == TRef TBool
                             then TBool
                             else TUnit
             Seq e1 e2 -> let
               t1 = vt ctx e1
               t2 = vt ctx e2 in if (t1 == TUnit)
                                 then t2
                                 else error $ "La primer expresión no es de tipo TUnit"
             While e1 e2 -> let
               t1 = vt ctx e1
               t2 = vt ctx e2 in if(t1 == TBool && t2 == TUnit)
                                 then TUnit
                                 else (if (t1 /= TBool)
                                       then error $ "La bandera  no es de tipo TBool"
                                       else error $ "El cuerpo no es de tipo TUnit")
             Unit -> TUnit
             Var x -> case ctx of
                        ([],ctxL) -> error $ "La variable "++show x++" no está en el contexto"
                        ((y,t):ctxV,ctxL) -> if x==y
                                             then t
                                             else vt (ctxV,ctxL) e

{-Aquí van tus pruebas para la semántica estática-}
prueba0' = vt([],[]) $ Let "x" (Ref $ VNum 3) (Suma (VNum 4) (Deref $ Var "x"))
prueba1' = vt ([("x",TRef TInt),("y",TRef TBool)],[]) $ Ifte (Menor (Deref (Var "x")) (Prod (Suma (VNum 3) (Deref (Var "x"))) (VNum 4)))(Iszero (VNum 3))(Eq (Suma (Prod (Deref(Var "x")) (VNum 5)) (Deref(Var "x"))) (Deref(Var"x")))
prueba2' = vt ([("x",TRef TInt),("y",TRef TInt),("z",TRef TBool)],[]) $ While (Iszero (Ifte (Deref(Var "z")) (Suma (Deref(Var "x")) (VNum 9)) (Prod (Deref(Var "y")) (VNum 9)))) (Let "z" (Ref (Deref (Var "x"))) (Seq (Asig (Var "x") (Deref (Var "y"))) (Asig (Var "y") (Deref (Var "z")))))
prueba3' = vt ([("x",TRef TInt)],[(L 0,TInt),(L 1,TBool)]) $ Let "y" (Ref $ VNum 3) (Ifte (Neg (Var "y")) (L 1) (L 0))
prueba4' = vt ([("x",TRef TInt)],[(L 0,TInt),(L 1,TBool)]) $ Let "y" (Ref $ VNum 3) (Ifte (L 0) (L 1) (L 0))
prueba5' = vt ([("y",TRef TInt)],[(L 0,TInt)]) $ Let "x" (Ref (VNum 3)) (Seq (Asig (Var "x") (Suma (Deref(L 0)) (Deref (Var "y")))) (Asig (Var "y") (Prod (VNum 3) (Deref(Var "x")))))
