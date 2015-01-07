{-# HADDOCK Markdown #-}
{- |
Module      : Function
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : BSD
Maintainer  : %myname% %mail%
Stability   : test
Portability : POSIX

(必須) モジュールの説明を書く: テストモジュール
-}

module Function (
   -- * function
   square
   -- * data
   -- ** 型式無し
  ,Color(Red, Blue)
   -- ** 型式有り
  ,Coordinate(Coordinate)
  -- * type
  ,TimeSeries
  -- * newtype
  ,Natural
) where


-- | (必須) 関数の説明を書く: 引数の2乗を返す
--
-- 複数行書ける
--
-- @
--  (任意) コードを書く: square x = x * x
-- @
--
-- >>> (任意) 使用例を書く: square 3
-- 9
square :: Int -- ^ (推奨) 引数の説明: 入力
       -> Int -- ^ (推奨) 返り値の説明: 出力
square x = x * x


-- | (必須) 型の説明: 
data Color = Red -- ^ (任意) 構成子の説明: 赤色 
           | Blue -- ^ (任意) 構成子の説明: 青色 

-- | (必須) 型の説明: 座標データ
data Coordinate = Coordinate { zahyoX :: Double -- ^ フィールドの説明。何故か出ない
                             , zahyoY :: Double -- ^ フィールドの説明。何故か出ない
                             } -- ^ (必須) 構成子の説明: Coordinate x座標 y座標


-- | (必須) 型の説明: 時系列データ [(t, h(t))]
type TimeSeries = [(Double, Double)]


-- | (必須) 型の説明: 自然数
newtype Natural = MakeNatural Integer

