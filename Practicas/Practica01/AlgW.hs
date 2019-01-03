{-Facultad de Ciencias UNAM - Lenguajes de programación 2019-1 
      Vázquez Reyes C. Moisés
      Rafael de Jesús García García 
      Magallanes Ramírez Diego Alberto
-}


-- Araujo Chavez Mauricio 312210047
-- Carmona Mendoza Martin 313075977

module AlgW where

import Data.List (union)
import Unificacion

{-----------------------}
--CÁLCULO LAMBDA PURO--
{-----------------------}
data Lam_U = VarU Nombre | LamU Nombre Lam_U | AppU Lam_U Lam_U 

instance Show Lam_U where
         show t = case t of
                   VarU x -> x
                   LamU x e -> "λ"++x++"."++show e
                   AppU (VarU x) (VarU y) -> x++y 
                   AppU (VarU x) e2 -> x++"("++show e2++")"
                   AppU e1 (VarU y) -> "("++show e1++")"++y
                   AppU e1 e2 -> "("++show e1++")"++"("++show e2++")"


--Función que lleva un término a su forma normal.
formaNormal :: Lam_U -> Lam_U
formaNormal (VarU x) = VarU x
formaNormal (LamU n e) = LamU n (formaNormal e)
formaNormal (AppU e1 e2) = betaR (AppU (formaNormal e1) (formaNormal e2))   

--Aplica la β-reducción de dos términos
betaR :: Lam_U -> Lam_U
betaR (AppU e1 e2) = case e1 of
  (LamU n e3) -> formaNormal (sustituye n e3 e2) where
    sustituye x (VarU v) expr2
      | (x == v) = expr2
      | otherwise = VarU v
    sustituye x (LamU v expr3) expr2 = LamU v (sustituye x expr3 expr2)
    sustituye x (AppU expr3 expr4) expr2 = (AppU
                                            (sustituye x expr3 expr2)
                                            (sustituye x expr4 expr2))
  e1 -> (AppU e1 e2)

   
--Función que indica si un término contiene un redex.                                                          
redex::Lam_U->Bool
redex t = case t of
           VarU _ -> False
           LamU _ e -> redex e
           AppU (LamU _ _) _ -> True 
           AppU e1 e2 -> redex e1 || redex e2

--Dado n, crea el natural de Church n correspondiente.
churchN::Int->Lam_U
churchN n = LamU "s" $ LamU "z" $ auxChurch n where
  auxChurch 0 = VarU "z"
  auxChruch n = AppU (VarU "s") (auxChurch (n-1))
                                 
--Sucesor para naturales de Church.
suc::Lam_U
suc = (LamU "n'" (LamU "s'" (LamU "z'" (AppU (VarU "s'") (AppU (AppU (VarU "n'") (VarU "s'"))(VarU "z'"))))))    

--Suma de naturales de Church.
suma::Lam_U
suma = (LamU "n" (LamU "m" (AppU (AppU (VarU "n") suc) (VarU "m"))))    

--Numero Cero de Curch (Lo ocupamos para la multiplicacion)
cero = (LamU "s0" (LamU "z0" (VarU "z0")))

--Producto de naturales de Church.
producto::Lam_U
producto = (LamU "n" (LamU "m" (AppU (AppU (VarU "n") (AppU suma (VarU "m")))cero)))

--if-then-else.
ift::Lam_U
ift = (LamU "b" (LamU "t" (LamU "e" (AppU (AppU (VarU "b") (VarU "t")) (VarU "e"))))) 

{-Codifica los combinadores de punto fijo-}
curryRosser = error "Te toca"
turing = error "Te toca"
klop = error "Te toca"

{-Implementa la función factorial recursiva con cada combinador de punto fijo-}
factorial_curryRosser::Lam_U
factorial_curryRosser = error "Te toca"

factorial_Turing::Lam_U
factorial_Turing = error "Te toca"

factorial_Klop::Lam_U
factorial_Klop = error "Te toca"

{-PRUEBAS-}
prueba_curryRosser_1 = formaNormal $ AppU factorial_curryRosser (churchN 1)
prueba_curryRosser_2 = formaNormal $ AppU factorial_curryRosser (churchN 2)
prueba_curryRosser_3 = formaNormal $ AppU factorial_curryRosser (churchN 3)
prueba_curryRosser_4 = formaNormal $ AppU factorial_curryRosser (churchN 4)
prueba_curryRosser_5 = formaNormal $ AppU factorial_curryRosser (churchN 5)

prueba_Turing_1 = formaNormal $ AppU factorial_Turing (churchN 1)
prueba_Turing_2 = formaNormal $ AppU factorial_Turing (churchN 2)
prueba_Turing_3 = formaNormal $ AppU factorial_Turing (churchN 3)
prueba_Turing_4 = formaNormal $ AppU factorial_Turing (churchN 4)
prueba_Turing_5 = formaNormal $ AppU factorial_Turing (churchN 5)

prueba_Klop_1 = formaNormal $ AppU factorial_Klop (churchN 1)
prueba_Klop_2 = formaNormal $ AppU factorial_Klop (churchN 2)
prueba_Klop_3 = formaNormal $ AppU factorial_Klop (churchN 3)
prueba_Klop_4 = formaNormal $ AppU factorial_Klop (churchN 4)
prueba_Klop_5 = formaNormal $ AppU factorial_Klop (churchN 5)

{-----------------------}
--INFERENCIA DE TIPOS--
{-----------------------}
--Expresiones LamAB sin anotaciones de tipos.
data LamAB = VNum Int
     | VBool Bool
     | Var Nombre
     | Suma LamAB LamAB
     | Prod LamAB LamAB
     | Ifte LamAB LamAB LamAB
     | Iszero LamAB
     | Lam Nombre LamAB
     | App LamAB LamAB
     | Fix Nombre LamAB
     deriving Show
     
        
--Expresiones LamAB con anotaciones de tipos.
data LamABT = VNumT Int
     | VBoolT Bool
     | VarT Nombre
     | SumaT LamABT LamABT
     | ProdT LamABT LamABT
     | IfteT LamABT LamABT LamABT
     | IszeroT LamABT
     | LamT Nombre Tipo LamABT
     | AppT LamABT LamABT
     | FixT Tipo Nombre LamABT
     deriving Show


--Para representar un contexto de variables [(x1,T1),...,(xn,Tn)].
type Ctx = [(Nombre,Tipo)]
type VarTipo = Tipo


--Para representar juicios de tipado.
data Juicio = Deriv (Ctx,LamABT,Tipo)

instance Show Juicio where
    show (Deriv (ctx, e, t)) = show ctx++" ⊢ "++show e++" : "++show t




--Realiza la inferencia de tipos de una expresión LamABT
algoritmoW :: LamAB->Juicio
algoritmoW e = let (Deriv (ctx,e',t),_) = w e [] in Deriv (elimRep ctx,e',t) where
                                                           elimRep [] = []
                                                           elimRep (x:xs) = x:(filter (x/=) $ elimRep xs)      

--Genera un tipo nuevo
nuevoTipo :: [Tipo] -> Tipo
nuevoTipo tipos = buscaTipo 0 tipos where
  buscaTipo n tipos
    | elem (X ("X" ++ show n)) tipos = buscaTipo (n + 1) tipos
    | otherwise = X ("X" ++ show n)

--Busca tuplas con el mismo nombre en 2 contextos y "une" los tipos
buscaSimilares :: Ctx -> Ctx -> [(Tipo, Tipo)]
buscaSimilares ctx1 ctx2 = [(t, t') | (n, t) <- ctx1, (n', t') <- ctx2, n == n']

--Realiza la sustitucion en un tipo                           
sustTT :: Tipo -> [Sust] -> Tipo
sustTT t [] = t
sustTT t (s:ss) = sustTT (apSustT t s) ss

--Realiza la sustitucion en un contexto
sustCtx :: Ctx -> [Sust] -> Ctx
sustCtx ctx s = [(n, (sustTT t s)) | (n, t) <- ctx]

--Realiza la sustitucion en un termino
sustLT :: LamABT -> [Sust] -> LamABT
sustLT e s = case e of
  VNumT i -> VNumT i
  VBoolT b -> VBoolT b
  VarT x -> VarT x
  SumaT e1 e2 -> SumaT (sustLT e1 s) (sustLT e2 s)
  ProdT e1 e2 -> ProdT (sustLT e1 s) (sustLT e2 s)
  IfteT e1 e2 e3 -> IfteT (sustLT e1 s) (sustLT e2 s) (sustLT e3 s)
  IszeroT e -> IszeroT (sustLT e s)
  LamT n t e -> LamT n (sustTT t s) (sustLT e s)
  AppT e1 e2 -> AppT (sustLT e1 s) (sustLT e2 s)

--Obtiene el tipo de un nombre de un contexto usando Maybe
obtenTipo :: Nombre -> Ctx -> Maybe Tipo
obtenTipo n [] = Nothing
obtenTipo n1 ((n2, t):cs)
  | (n1 == n2) = Just t
  | otherwise = obtenTipo n1 cs

--Elimina la variable y su tipo de un contexto
elimVarT :: (Nombre, Tipo) -> Ctx -> Ctx
elimVarT (n, t) ctx = [(n', t') | (n', t') <- ctx, n' /= n, t' /= t]


--Realiza el algoritmo W en una expresión LamAB utilizando una lista de nombres que ya están ocupados. 
w::LamAB->[VarTipo]->(Juicio,[VarTipo])                
w (VNum i) vars = (Deriv ([], VNumT i, TNat), vars)
w (VBool b) vars = (Deriv ([], VBoolT b, TBool), vars)
w (Var x) vars = (Deriv ([(x, nT)], VarT x, nT), union vars [nT]) where
  nT = nuevoTipo vars
w (Suma e1 e2) vars = (Deriv (ctx', SumaT e1' e2', TNat), vars') where
  (Deriv (ctx1, ex1, t1), vars1) = w e1 vars
  (Deriv (ctx2, ex2, t2), vars2) = w e2 vars1
  umg = unificaConj ((buscaSimilares ctx1 ctx2) ++ [(t1, TNat), (t2, TNat)])
  ctx' = union (sustCtx ctx1 umg) (sustCtx ctx2 umg)
  e1' = sustLT ex1 umg
  e2' = sustLT ex2 umg
  vars' = vars2
w (Prod e1 e2) vars = (Deriv (ctx', ProdT e1' e2', TNat), vars') where
  (Deriv (ctx1, ex1, t1), vars1) = w e1 vars
  (Deriv (ctx2, ex2, t2), vars2) = w e2 vars1
  umg = unificaConj ((buscaSimilares ctx1 ctx2) ++ [(t1, TNat), (t2, TNat)])
  ctx' = union (sustCtx ctx1 umg) (sustCtx ctx2 umg)
  e1' = sustLT ex1 umg
  e2' = sustLT ex2 umg
  vars' = vars2
w (Ifte e1 e2 e3) vars = (Deriv (ctx', IfteT e1' e2' e3', t'), vars') where
  (Deriv (ctx1, ex1, t1), vars1) = w e1 vars
  (Deriv (ctx2, ex2, t2), vars2) = w e2 vars1
  (Deriv (ctx3, ex3, t3), vars3) = w e3 vars2
  similaresI = union (buscaSimilares ctx1 ctx2) (buscaSimilares ctx1 ctx3)
  similaresF = union similaresI (buscaSimilares ctx2 ctx3)
  umg = unificaConj (similaresF ++ [(t1, TBool), (t2, t3)])
  ctx' = union (union (sustCtx ctx1 umg) (sustCtx ctx2 umg)) (sustCtx ctx3 umg) 
  e1' = sustLT ex1 umg
  e2' = sustLT ex2 umg
  e3' = sustLT ex3 umg
  t' = sustTT t2 umg
  vars' = vars3
w (Iszero e) vars = (Deriv (ctx', IszeroT e', TBool), vars') where
  (Deriv (ctx1, ex, t1), vars1) = w e vars
  umg = unifica TBool t1
  ctx' = sustCtx ctx1 umg
  e' = sustLT ex umg
  vars' = vars1
w (Lam n e) vars = (Deriv (elimVarT (n, t') ctx, LamT n t' e', t' :-> t),
                    union vars' [t']) where
  t' = case (obtenTipo n ctx) of
    Nothing -> nuevoTipo vars'
    Just tipo -> tipo
  (Deriv (ctx, e', t), vars') = w e []
w (App e1 e2) vars = (Deriv (ctx', AppT e1' e2', t'), vars') where
  (Deriv (ctx1, ex1, t1), vars1) = w e1 vars
  (Deriv (ctx2, ex2, t2), vars2) = w e2 vars1
  nT = nuevoTipo vars2
  umg = unificaConj ((buscaSimilares ctx1 ctx2) ++ [(t1, t2 :-> nT)])
  ctx' = union (sustCtx ctx1 umg) (sustCtx ctx2 umg)
  e1' = sustLT ex1 umg
  e2' = sustLT ex2 umg
  t' = sustTT nT umg
  vars' = union vars2 [nT]

{-PRUEBAS:-}

-- []|-LamT "x" X1 (LamT "y" X0 (VarT "y")):X1->(X0->X0)
prueba1 = algoritmoW $ Lam "x" $ Lam "y" $ Var "y"

-- [("x",X3->(X4->X0)),("y",X3),("z",X4)]|-AppT (AppT (VarT "x") (VarT "y")) (VarT "z"):X0
prueba2 = algoritmoW $ App (App (Var "x") (Var "y")) (Var "z")

-- *** Exception: No se pudo unificar.
prueba3 = algoritmoW $ App (Var "x") (Var "x")

-- []|-LamT "s" X2->X0 (LamT "z" X2 (AppT (VarT "s") (VarT "z"))):(X2->X0)->(X2->X0)
prueba4 = algoritmoW $ Lam "s" $ Lam "z" $ App (Var "s") (Var "z")

-- [("x",X6->(X4->X0)),("z",X6),("y",X6->X4),("z",X6)]|-AppT (AppT (VarT "x") (VarT "z")) (AppT (VarT "y") (VarT "z")):X0
prueba5 = algoritmoW $ App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))

-- []|-LamT "f" Nat->X0 (LamT "x" Nat (LamT "y" Nat (AppT (VarT "f") (SumaT (VarT "x") (VarT "y"))))):(Nat->X0)->(Nat->Nat->X0)
prueba6 = algoritmoW $ Lam "f" $ Lam "x" $ Lam "y" $ App (Var "f") (Suma (Var "x") (Var "y")) 

-- [("g",X2->X0),("f",Nat->X2),("z",Nat)]|-AppT (VarT "g") (AppT (VarT "f") (ProdT (VNumT 3) (VarT "z"))):X0
prueba7 = algoritmoW $ App (Var "g") (App (Var "f") (Prod (VNum 3) (Var "z")))

-- [("f",X2->Bool),("y",X2)]|-IfteT (IszeroT (SumaT (VNumT 2) (VNumT 0))) (AppT (VarT "f") (VarT "y")) (VBoolT False):Bool
prueba8 = algoritmoW $ Ifte (Iszero $ Suma (VNum 2) (VNum 0)) (App (Var "f") (Var "y")) (VBool False)

-- [("f",X2->X3)]|-LamT "x" X2 (LamT "y" X3 (IfteT (VBoolT True) (AppT (VarT "f") (VarT "x")) (VarT "y"))):X2->(X3->X3)
prueba9 = algoritmoW $ Lam "x" $ Lam "y" $ Ifte (VBool True) (App (Var "f") (Var "x")) (Var "y")
 
-- *** Exception: No se pudo unificar.
prueba10 = algoritmoW $ App (Suma (VNum 1) (Var "n")) (Var "w")



{-----------------------}
--SEMÁNTICA DINÁMICA--
{-----------------------}


     
--Semántica dinámica en un paso.
--eval1::LamABT->LamABT
--eval1 t = error "Te toca"
                                    

--Evaluación de un 'asa' cuando es un valor.
--eval::LamABT->Either Int Bool
--eval t = case t of
  --        VNumT n -> n
    --      VBoolT b -> b
      --    t' -> error "Te toca"












