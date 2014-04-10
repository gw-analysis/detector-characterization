-- 課題1.1 facttorialをそれぞれの文で書き直す

 -- ガードを用いた記法
factorialguard :: Int -> Int
factorialguard n
  | n < 0 = error "why did you insert a Minos number?"
  | n == 0 = 1
  | otherwise = n * factorialguard (n - 1)

 -- if文を用いた記法
factorialif :: Int -> Int
factorialif n = if n < 0
                then error "why did you insert a Minos number?"
                else if n == 0
                     then 1
                     else n*factorialif (n - 1)

 -- case式を用いた方法
factorialcase :: Int -> Int
factorialcase n = case n < 0 of True -> error "Why did you insert a Minos number?"
                                False -> factorialfunc n
  where factorialfunc n = case n of 0 -> 1
                                    otherwise ->  n*factorialfunc (n - 1)

-- 課題1.2 うるう年か判断するコード 
uruucal :: Int -> String
uruucal xs
  | mod xs 400 == 0 = "uruu!"
  | mod xs 100 == 0 = "non uruu..."
  | mod xs 4 == 0 = "uruu!"
  | otherwise = "non uruu..."

-- 課題1.3 

-- 以下、論述問題のため全てコメントアウトにて記述する
-- なお、全ての引数をargとして記述する

-- (a)
-- pair(f,g) argで(f arg,g arg)と変化する。よって
-- fst pair(f,g) arg = f arg

-- (b)
-- snd pair(f,g) arg = g arg

-- (c)
-- pair(f,g).h arg = (f.h arg,g.h arg)
--                 = pair(f.h,g.h) arg

-- (d)
-- cross(f,g).pair(h,k) arg = pair(f.fst,g.snd).pair(h,k) arg
--                          = pair(f.fst,g.snd)(h arg,k,arg)
--                          = (f.fst(h arg,k arg),g.snd(h arg,k arg))
--                          = (f.h arg,g,k arg)
--                          = pair(f.h,g.k) arg

-- (e)
-- cross(f,g).cross(h,k) arg = pair(f.fst,g.snd).pair(h.fst,k.snd) arg
--                           = pair(f.fst,g.snd)(h.fst arg,k.snd arg)
--                           = (f.h.fst arg, g.k.snd arg)
--                           = cross(f.h,g,k) arg
