divisaoSegura :: Integer -> Integer -> Maybe Integer
divisaoSegura x 0 = Nothing
divisaoSegura x y = Just(div x y)

divisaoSegura' :: Integer -> Integer -> Either Integer String
divisaoSegura' x 0 = Right((show x) ++ "/0")
divisaoSegura' x y = Left(div x y)

mapMaybe :: (a -> Maybe b) -> [a] -> [Maybe b]
mapMaybe f a = filter (isJust) (map f a)
  where
    isJust :: Maybe b -> Bool
    isJust x = case x of
      Nothing -> False
      Just _ -> True

classifica :: [Either a b] -> ([a], [b])
classifica lista = classifica' lista [] []
  where
    classifica' :: [Either a b] -> [a] -> [b] -> ([a], [b])
    classifica' [] a b = (a, b)
    classifica' (l:ls) a b = case l of
      Left x -> classifica' ls (a ++ [x]) b
      Right x -> classifica' ls a (b ++ [x])