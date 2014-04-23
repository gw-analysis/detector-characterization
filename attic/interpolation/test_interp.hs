import Bindings.Gsl.Interpolation
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.C.Types(CSize, CInt, CDouble)
import Foreign.Storable(peek)
import Foreign.Marshal.Array

--type Interpolation = Ptr (Ptr gsl_interp_type)
main :: IO ()
main = do


  let ndat = 10 :: CSize
      ndatCD = realToFrac ndat :: CDouble
      x = map (\a -> a + 0.5 * sin a) [0..ndatCD-1]
      y = map (\a -> a + cos (a * a)) [0..ndatCD-1]
      --len = length x
  ptr_x <- withArrayLen x $ \len ptr_tmp -> do
              return ptr_tmp
  ptr_y <- withArrayLen y $ \len ptr_tmp -> do
              return ptr_tmp

  let interp_x = x!!5+0.01


  -- allocate memory
  --ptr_gsl_interp_accel :: Ptr C'gsl_interp_accel
  ptr_gsl_interp_accel <- c'gsl_interp_accel_alloc

  -- generate instance of interpolation type gsl_interp_cspline, size 10
  -- select type of interpolation
  ptr_gsl_spline <- peek p'gsl_interp_cspline

  -- ptr_spline :: Ptr_C'gsl_spline
  ptr_spline <- c'gsl_spline_alloc ptr_gsl_spline ndat

  -- initialize gsl_spline
  retval <- c'gsl_spline_init ptr_spline ptr_x ptr_y ndat

  -- perform the interpolation
  interp_y <- c'gsl_spline_eval ptr_spline interp_x ptr_gsl_interp_accel

  -- freeing allocated memory
  c'gsl_spline_free ptr_spline
  c'gsl_interp_accel_free ptr_gsl_interp_accel

  -- display the output
  putStrLn $ show $ realToFrac interp_y

