fibo n = fib 0 1 n
fib _ _ 0 = []
fib a b n = a+b:fib b (a+b) (n-1)

fibona 0 = []
fibona a = let gold = 1.61803398875 in round (gold ** fromIntegral a / sqrt 5.0) : fibona (a - 1)