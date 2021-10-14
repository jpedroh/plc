menorMaior :: Integer -> Integer -> Integer -> (Integer, Integer)
menorMaior x y z = ((min x (min y z)), (max x (max y z)))

ordenaTripla :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
ordenaTripla (a, b, c)
  | a > b && a > c = (a, fst (ordenaDupla (b, c)), snd (ordenaDupla (b, c)))
  | b > a && b > c = (b, fst (ordenaDupla (a, c)), snd (ordenaDupla (a, c)))
  | otherwise = (c, fst (ordenaDupla (a, b)), snd (ordenaDupla (a, b)))
    where
      ordenaDupla :: (Integer, Integer) -> (Integer, Integer)
      ordenaDupla (b, c)
        | b > c = (b, c)
        | otherwise = (c, b)

type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

primeiraCoordenada :: Ponto -> Float
primeiraCoordenada p = fst p

segundaCoordenada :: Ponto -> Float
segundaCoordenada p = snd p

ehVertical :: Reta -> Bool
ehVertical r = primeiraCoordenada (fst r) == primeiraCoordenada (snd r)

pontoY :: Float -> Reta -> Float
pontoY p r = (m * (p - (primeiraCoordenada (fst r)))) + (segundaCoordenada (fst r))
  where
      m = (segundaCoordenada (snd r) - segundaCoordenada (fst r)) / (primeiraCoordenada (snd r) - primeiraCoordenada (fst r))