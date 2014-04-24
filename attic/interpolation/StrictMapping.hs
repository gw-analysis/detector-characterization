

module StrictMapping
(mapM',
forM'
) where

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' _ []     = return []
mapM' f (x:xs) = do y  <- f x
                    ys <- y `seq` mapM' f xs
                    return (y:ys)

forM' :: Monad m => [a] -> (a -> m b) -> m [b]
forM' = flip mapM'
