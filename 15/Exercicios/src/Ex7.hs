module Ex7 where
    concatMap2 :: (a -> [b]) -> [a] -> [b]
    concatMap2 f = concat . map f