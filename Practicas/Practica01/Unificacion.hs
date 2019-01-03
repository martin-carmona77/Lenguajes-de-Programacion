{-Facultad de Ciencias UNAM - Lenguajes de programación 2019-1 
      Vázquez Reyes C. Moisés
      Rafael de Jesús García García 
      Magallanes Ramírez Diego Alberto
-}

module Unificacion where

infixr :-> {- Así, el poderador ':->' asocia a la derecha. -}
type Nombre = String

-- Categoría de tipos.
data Tipo = TNat | TBool | X Nombre | Tipo :-> Tipo deriving Eq


instance Show Tipo where
     show t = case t of
            TNat -> "ℕ"
            TBool -> "𝔹"
            X name -> name 
            TNat:->TNat -> "ℕ" ++"->"++"ℕ"
            TNat:->TBool -> "ℕ" ++"->"++"𝔹"
            TNat:->(X name) -> "ℕ"++"->"++name
            TNat:->(t1:->t2) -> "ℕ"++"->("++show t1++"->"++show t2++")"
            TBool:->TBool -> "𝔹" ++"->"++"𝔹"
            TBool:->TNat -> "𝔹" ++"->"++"ℕ"
            TBool:->(X name) -> "Bool"++"->"++name
            TBool:->(t1:->t2) -> "𝔹"++"->("++show t1++"->"++show t2++")"
            (X name):->TNat -> name++"->"++"ℕ"
            (X name):->TBool -> name++"->"++"𝔹"
            (X name1):->(X name2) -> name1++"->"++name2
            (X name):->(t1:->t2) -> name++"->("++show t1++"->"++show t2++")"
            (t1:->t2):->TNat -> "("++show t1++"->"++show t2++")"++"->"++"ℕ"
            (t1:->t2):->TBool -> "("++show t1++"->"++show t2++")"++"->"++"𝔹"
            (t1:->t2):->(X name) -> "("++show t1++"->"++show t2++")"++"->"++name
            (t1:->t2):->(t3:->t4) -> "("++show t1++"->"++show t2++")"++"->("++show t3++"->"++show t4++")"


--Una sustitución es un conjunto de la forma [(xi, Ti)]
type Sust = [(Nombre, Tipo)]


--Elimina sustituciones de la forma [X:=X] en una sustitución.
simpSust::Sust->Sust
simpSust [] = []
simpSust ((x,t):s) = case t of
            X y -> if x==y then simpSust s else ((x,t)):(simpSust s)
            t -> ((x,t)):(simpSust s)   
                     

--Realiza la composición de dos sustituciones.
compSust::Sust->Sust->Sust
compSust s1 s2 = simpSust [ (x, apSustT t s2) | (x,t) <- s1] ++ [ (y,t) | (y,t) <- s2, not $ elem y $ [x | (x,t) <- s1]] 


--Aplica una sustitución a un tipo.
apSustT::Tipo->Sust->Tipo 
apSustT t sust = case t of
            TNat -> TNat
            TBool -> TBool
            X x -> case sust of
                          [] -> X x 
                          ((y,t1):sust1) -> if x==y then t1 else apSustT (X x) sust1
            t1 :-> t2 -> apSustT t1 sust :-> apSustT t2 sust


--Unifica dos tipos.
unifica :: Tipo -> Tipo -> [Sust]
unifica TNat TNat = [[]]
unifica TNat t = error ("No se puede unificar TNat con " ++ show t)
unifica TBool TBool = [[]]
unifica TBool t = error ("No se puede unificar TBool con " ++ show t)
unifica (X x1) (X x2)
  | (x1 == x2) = [[]]
  | otherwise = [[(x1, X x2)]]
unifica (X x) t
  | (elem x (varT t)) = error ("No se puede unificar, ya que " ++ x ++
                               " figura en " ++ show t)
  | otherwise = [[(x, t)]] where
      varT t = case t of
        (X x) -> [x]
        (t1 :-> t2) -> (varT t1) ++ (varT t2)
        t -> []
unifica t (X x) = unifica (X x) t
unifica (t1 :-> t2) (t3 :-> t4) = [(compSust sust1 sust2) |
                                   sust1 <- (unifica t1 t3),
                                   sust2 <- (unifica
                                             (apSustT t2 sust1)
                                             (apSustT t4 sust1))]        


--Unifica una lista de tipos.
unificaConj::[(Tipo,Tipo)]->[Sust]
unificaConj [] = [[]]
unificaConj ((t1,t2):ts) = [compSust s1 s2 | s1 <- unifica t1 t2, s2 <- unificaConj [(apSustT (fst t) s1,apSustT (snd t) s1) | t <- ts]]



