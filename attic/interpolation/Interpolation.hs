
module Interpolation
  (
  interp
  ) where


import Bindings.Gsl.Interpolation
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.C.Types(CSize, CInt, CDouble)
import Foreign.Storable(peek)
import Foreign.Marshal.Array

--type Interpolation = Ptr (Ptr gsl_interp_type)
--main :: IO ()
--main = do


--  let ndat = 10 :: CSize
--      ndatCD = realToFrac ndat :: CDouble
--      x = map (\a -> a + 0.5 * sin a) [0..ndatCD-1]
--      y = map (\a -> a + cos (a * a)) [0..ndatCD-1]
--      --len = length x
--  ptr_x <- withArrayLen x $ \len ptr_tmp -> do
--              return ptr_tmp
--  ptr_y <- withArrayLen y $ \len ptr_tmp -> do
--              return ptr_tmp
--
--  let interp_x = x!!5+0.01

data InterpolationType = Spline | Linear

interp :: [Double] -> [Double] -> Double -> InterpolationType -> IO Double
interp xx yy interp_xx interpType = do
  let x = d2cd xx
      y = d2cd yy
      interp_x = realToFrac interp_xx :: CDouble

  let ndat = fromIntegral ( length x) :: CSize
  ptr_x <- withArrayLen x $ \len ptr_tmp -> do
              return ptr_tmp
  ptr_y <- withArrayLen y $ \len ptr_tmp -> do
              return ptr_tmp

  -- allocate memory
  --ptr_gsl_interp_accel :: Ptr C'gsl_interp_accel
  ptr_gsl_interp_accel <- c'gsl_interp_accel_alloc

  -- generate instance of interpolation type gsl_interp_cspline, size 10
  -- select type of interpolation
  ptr_interp_type <- case interpType of
      Spline -> do
        ptr_gsl_interp_type <- peek p'gsl_interp_cspline
        --ptr_interp_type :: Ptr_C'gsl_spline
        c'gsl_spline_alloc ptr_gsl_interp_type ndat
      Linear -> do
        ptr_gsl_interp_type <- peek p'gsl_interp_linear
        c'gsl_spline_alloc ptr_gsl_interp_type ndat


  -- initialize gsl_spline
  retval <- c'gsl_spline_init ptr_interp_type ptr_x ptr_y ndat

  -- perform the interpolation
  interp_y <- c'gsl_spline_eval ptr_interp_type interp_x ptr_gsl_interp_accel

  -- freeing allocated memory
  c'gsl_spline_free ptr_interp_type
  c'gsl_interp_accel_free ptr_gsl_interp_accel

  -- display the output
  --putStrLn $ show $ realToFrac interp_y
  return (realToFrac interp_y :: Double)


d2cd :: [Double] -> [CDouble]
d2cd [] = []
--d2cd (x:xs) = (realToFrac x :: CDouble) : d2cd xs
d2cd x = map realToFrac x







