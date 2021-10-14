paraMaiuscula :: String -> String
paraMaiuscula s = [ toUppercase x | x <- s ]
  where
    toUppercase :: Char -> Char
    toUppercase x
      | x >= 'a' && x <= 'z' = toEnum (fromEnum x - 32)
      | otherwise = x

divisores :: Integer -> [Integer]
divisores x = [ y | y <- [1..x], x `mod` y == 0 ]

isPrime :: Integer -> Bool
isPrime x = (divisores x) == [1, x]

menorLista :: [Integer] -> Integer
menorLista x = menorLista' (head x) (tail x)
  where
    menorLista' :: Integer -> [Integer] -> Integer
    menorLista' x lista
      | lista == [] = x
      | x < (head lista) = menorLista' x (tail lista)
      | otherwise = menorLista' (head lista) (tail lista)

measure :: [t] -> Integer
measure [] = -1
measure [x] = 1
measure xs = 1 + measure (tail xs)

takeFinal :: [t] -> Int -> [t]
takeFinal lista n = snd (splitAt ((length lista) - n) lista)

remove :: Int -> [t] -> [t]
remove 0 lista = tail lista
remove n lista = fst parte ++ tail (snd parte)
  where
    parte = splitAt n lista
  
exercicioOito :: [Integer] -> Integer
exercicioOito [] = 0
exercicioOito (x: xs) = x + 1

exercicioOito' :: [Integer] -> Integer
exercicioOito' lista
  | length lista == 0 = 0
  | otherwise = head lista + 1

exercicioNove :: [Integer] -> Integer
exercicioNove (a:b:c) = a + b
exercicioNove (a:b) = a
exercicioNove [] = 0

produto :: [Integer] -> Integer
produto [] = 1
produto (x:cauda) = x * produto(cauda)

unique :: [Integer] -> [Integer]
unique lista = [ x | x <- lista, (contaItem x lista) == 1 ]
  where
    contaItem :: Integer -> [Integer] -> Integer
    contaItem x [] = 0
    contaItem x (y:ys)
      | x == y = 1 + contaItem x ys
      | otherwise = contaItem x ys

ehCrescente :: [Integer] -> Bool
ehCrescente [] = True
ehCrescente [a] = True
ehCrescente (a:b:c)
  | a > b = False
  | otherwise = ehCrescente ([b] ++ c)