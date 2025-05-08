pertence a [] = False
pertence a (x:xs) = if a == x then True else pertence a xs

intercessao [] xs = []
intercessao xs [] = []
intercessao (x:xs) ys = if (pertence x ys) then [x] ++ intercessao xs ys else intercessao xs ys

inverso [] = []
inverso (x:xs) = inverso xs ++ [x]

nUltimos a xs = nPrimeiros a (inverso xs)
nPrimeiros _ [] = []
nPrimeiros 0 _ = []
nPrimeiros a (x:xs) = nPrimeiros (a - 1) xs ++ [x]

soma2 (x:xs) [] = []
soma2 [] (y:ys) = []
soma2 (x:xs) (y:ys) = ((y+x):(soma2 xs ys))

pot2 a = inverso (potencia2 a)
potencia2 0 = []
potencia2 a = ((2^a):potencia2 (a-1))  

intercalacao [] ys = ys
intercalacao (x:xs) ys = x:intercalacao ys xs

menor [x] = x
menor (x:xs) = if x < menor xs then x else menor xs

removerElem a [] = []
removerElem a (x:xs) = if a == x then xs else x:(removerElem a xs)

ordenar [] = []
ordenar xs = let min = menor xs in [min] ++ ordenar (removerElem min xs)

ins a [] = [a]
ins a (x:xs) = if (a < x) then (a : (x:xs)) else (x : ins a xs)  

enesimo 1 (x:xs) = x
enesimo a (x:xs) = enesimo (a-1) xs

repetir 0 b = []
repetir a b = b:(repetir (a-1) b)

numString 0 = []
numString a =  numString (div a 10) ++ [int2char (mod a 10)]
int2char:: Int -> Char
int2char n = toEnum (n + 48)

stringNum [] = 0
stringNum (x:xs) = (char2int x)*10^(length (x:xs)-1)+stringNum xs
char2int::Char -> Int
char2int a = fromEnum a-48

bin2int xs = bin2int' xs ((length xs)-1)
bin2int' [] (-1) = 0
bin2int' (x:xs) a = if x == '1' then (2^a + bin2int' xs (a-1)) else (0 + bin2int' xs (a-1))

int2bin a = inverso (int2bin' a)
int2bin' 0 = []
int2bin' a = if mod a 2 == 0 then ['0'] ++ (int2bin' (div a 2)) else ['1'] ++ (int2bin' (div a 2))

minusculas [] = []
minusculas (x:xs) = let a=(toint x) in if (a>=65 && a<=90) then (tochar (a+32):(minusculas xs)) else (x:(minusculas xs))

toint::Char -> Int
toint a = fromEnum a
tochar::Int -> Char
tochar a = toEnum a
