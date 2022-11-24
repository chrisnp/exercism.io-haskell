module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong = (==) <*> (sum . raised . digits)

raised :: Integral a => [a] -> [a]
raised = 
    let 
        raise = flip fmap 
    in 
        raise <*> (flip (^) . length)

digits :: Integral a => a -> [a]
digits x 
    | x < 1     = []
    | otherwise = digits (x `div` 10) ++ [x `mod` 10]