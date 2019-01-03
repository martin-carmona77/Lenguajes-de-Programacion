{-
Facultad de Ciencias, UNAM - Lenguajes de programación 2019-1
C. Moisés Vázquez Reyes
Rafael de Jesús García García
Diego Alberto Magallanes Ramírez	
-}

-- | Araujo Chavez Mauricio 312210047
-- | Carmona Mendoza Martín 313075977

--Números naturales
data N = Cero | S N 

--1 ================================================
instance Eq N where
   Cero == Cero = True
   (S n) == (S m) = n == m
   _ == _ = False

instance Ord N where
  Cero <= _ = True
  (S n) <= (S m) = n <= m
  _ <= _ = False

--2 ================================================
naturales :: N -> Int
naturales Cero = 0
naturales (S n) = 1 + (naturales n)

instance Show N where
   show n = show (naturales n) ++ "_N" 
   
--3 ================================================                   
creaN :: Int -> N
creaN 0 = Cero
creaN n = S(creaN(n-1))


sumN :: N -> N -> N
sumN Cero x = x
sumN (S n) m = S(sumN n m)


prodN :: N -> N -> N
prodN n Cero = Cero
prodN n (S m) = sumN n (prodN n m)


menorN :: N -> N -> Bool
menorN n m = n < m 

--4 ================================================
equivR :: (N,N) -> (N,N) -> Bool
equivR (a,b) (c,d) = (sumN a d) == (sumN b c)


--5 ================================================
data Z = Clase (N,N) deriving Show

instance Eq Z where
  Clase a == Clase b = equivR a b
    
    
--6 ================================================
creaZ :: Int -> Z
creaZ n = if (n >= 0)
          then Clase (creaN n, Cero)
          else Clase (Cero, creaN (-n))

sumZ :: Z -> Z -> Z
sumZ (Clase(a,b)) (Clase(c,d)) = Clase (sumN a c, sumN b d)


prodZ :: Z -> Z -> Z
prodZ (Clase(a,b)) (Clase(c,d)) = Clase (sumN (prodN a c) (prodN b d), sumN (prodN a d) (prodN b c))


ceroZ :: Z -> Bool
ceroZ (Clase (a,b)) = a == b

positivoZ :: Z -> Bool
positivoZ (Clase(a,b)) = if(a > b)
                         then True
                         else False 
negativoZ :: Z -> Bool
negativoZ (Clase(a,b)) = if(a < b)
                         then True
                         else False

--7 ================================================
pyu :: [a] -> (a,a)
pyu [] = error "Lista sin elementos"
pyu x = (head x, last x)

--8 ================================================
clona ::[Int] -> [Int]
clona [] = []
clona (x:xs) = (take x(repeat x)) ++ (clona xs)

--9 ================================================
agrupa :: [Int] -> [[Int]]
agrupa [] = []
agrupa [x] = [[x]]
agrupa (x:xs)
  | (elem x listacabeza) = (x:listacabeza):listacola
  | otherwise = [x]:sublista where
      sublista = agrupa xs
      listacabeza = head sublista
      listacola = tail sublista
               
--10 ================================================ 
cuentaAp :: Int -> [Int] -> Int
cuentaAp a [] = 0
cuentaAp a (x:xs) = if (a == x)
                    then (1 + cuentaAp a xs)
                    else (cuentaAp a xs)

elimRe :: [Int] -> [Int]
elimRe [] = []
elimRe (x:xs) = if (elem x xs)
                then (x : elimRe xs)
                else (elimRe xs)

frec :: [Int] -> [(Int,Int)]
frec l = [(x, (cuentaAp x l))| x <- (elimRe l)] 

--11 ================================================
elimina :: Int -> [Int] -> [Int]
elimina n xs = [ x | x <- xs, x `mod` n /= 0 ]

criba :: [Int] -> [Int]
criba [] = []
criba (n:ns) = n : criba (elimina n ns)

primos :: Int -> [Int]
primos n = (take n (criba [2..(n-1)]))
