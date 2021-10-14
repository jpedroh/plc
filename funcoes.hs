dobro :: Integer -> Integer
dobro x = 2 * x

quadruplo :: Integer -> Integer
quadruplo x = dobro (dobro x)

poli2 :: Double -> Double -> Double -> Double -> Double
poli2 a b c x = (a * x * x) + (b * x) + c

parImpar :: Integer -> String
parImpar x
  | ehPar x = "par"
  | otherwise = "impar"
  where
      ehPar :: Integer -> Bool
      ehPar x = x `mod` 2 == 0

maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour x y z w = maxThree x y (maxThree y z w)
  where
    maxThree :: Integer -> Integer -> Integer -> Integer
    maxThree x y z
      | x > y && x > z = x
      | y > z = y
      | otherwise = z

maxFour' :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour' x y z w = max (max x y) (max z w)

maxFour'' :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour'' x y z w = maxThree x y (max z w)
  where
    maxThree :: Integer -> Integer -> Integer -> Integer
    maxThree x y z
      | x > y && x > z = x
      | y > z = y
      | otherwise = z

quantosIguais :: Integer -> Integer -> Integer -> Integer
quantosIguais x y z 
  | x == y && y == z = 3
  | x == y || y == z || x == z = 2
  | otherwise = 0

ehZero :: Integer -> Bool
ehZero 0 = True
ehZero n = False

sumTo :: Integer -> Integer
sumTo 1 = 1
sumTo n = n + sumTo (n - 1)

potencia :: Integer -> Integer -> Integer
potencia x 0 = 1
potencia x y = x * (potencia x (y - 1))

coeficienteBinomial :: Integer -> Integer -> Integer
coeficienteBinomial n 0 = 1
coeficienteBinomial 0 k = 0
coeficienteBinomial n k = (coeficienteBinomial (n - 1) k) + (coeficienteBinomial (n - 1) (k - 1))
