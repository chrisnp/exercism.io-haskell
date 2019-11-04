module ArmstrongNumbers (armstrong) where


digits :: Integral a => a -> [a]
digits x 
    | x < 1     = []
    | otherwise = digits (x `div` 10) ++ [x `mod` 10]
    
raised :: Integral a => [a] -> [a]
raised xs = raise xs (^ (length xs))
    where
        raise = flip fmap 

armstrong :: Integral a => a -> Bool
armstrong = (==) <*> (sum . raised . digits)
