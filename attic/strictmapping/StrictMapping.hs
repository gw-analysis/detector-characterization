


module StrictMapping.StrictMapping
(mapM',
forM'
) where

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f x = go x []
  where go []     acc = return $ reverse acc
        go (b:bs) acc = do y <- f b
                           go bs (y:acc)

forM' :: Monad m => [a] -> (a -> m b) -> m [b]
forM' = flip mapM'
