ehTriangulo a b c = a + b > c && b + c > a && c + a > b

--------------------------------------------------------------------------------

tipoTriangulo a b c
 | a == b = (if b==c then "equilatero" else "isoceles") 
 | otherwise = (if b == c then "isoceles" else 
                (if a == c then "isoceles" else "escaleno"))

--------------------------------------------------------------------------------

triangulo a b c 
 | ehTriangulo a b c == True = (tipoTriangulo a b c) 
 | otherwise = "nao eh um triangulo"

--------------------------------------------------------------------------------

somaPares x = if rem x 2 == 0 then somando x else somando (x-1)
somando 0 = 0
somando y = y + somando (y-2)

--------------------------------------------------------------------------------

somaLista [] = 0 
somaLista (x:xs) = x + somaLista xs  

somaPot2m a b = do
        putStr (show a)
        potencias a b 1 (a:[])

potencias d e f g =
        if f <= e
                then do
                        putStr (" + "++show (d*(2^f)))
                        let n = d*(2^f)
                        potencias d e (f+1) (n:g)
                else do
                        putStrLn (" = "++(show (somaLista g)))

--------------------------------------------------------------------------------

isPrime :: Int -> Bool
isPrime n
  | n <= 1    = False  
  | n == 2    = True   
  | even n    = False  
  | otherwise = not (any divisible [3,5..(floor . sqrt $ fromIntegral n)])
  where
    divisible x = n `mod` x == 0

primoachar a b c
 | rem a c == 0 = False
 | a > b = True
 | otherwise = primoachar a b (c+2)

--------------------------------------------------------------------------------

seriePI n = termos n 1 0 1
termos n den som sinal
 | 4/den > 4/n = termos n (den+2) (som+(sinal*(4/den))) (-sinal)  
 | otherwise = som
 
 --------------------------------------------------------------------------------

seriePI2 0 = 4
seriePI2 n = (4 / fromIntegral(1 + n*2)) * (if mod n 2 == 0 then 1 else -1) + seriePI2 (n-1)

 --------------------------------------------------------------------------------

fat 0 = 1
fat a = a*fat (a-1)

fats n = [fat x | x <- [1..n]]

tet _ 0 = 1
tet x y = x^(tet x (y-1))

pen n m = tet (m-1) (tet n m) 