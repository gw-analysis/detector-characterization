
{-
 - calculation of maximal information coefficients
 -
 -}


{-# LANGUAGE ForeignFunctionInterface #-}


module HasKAL.ExternalUtils.Minepy.Mine
--(
--)
where


import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)


#incude "mine.h"


data C'Mine_problem = C'Mine_problem
  { n :: CInt
  , x :: Ptr CDouble
  , y :: Ptr CDouble
  }

data C'Mine_parameter = C'Mine_parameter
  { alpha :: CDouble
  , c  :: CDouble
  }

data C'Mine_score = C'Mine_score
  { n :: CInt
  , m :: Ptr CInt
  , mm:: Ptr (Ptr CDouble)
  }

data Mine_problem = Mine_problem
  { n :: Int
  , x :: [Double]
  , y :: [Double]
  }

data Mine_parameter = Mine_parameter
  { alpha :: Double
  , c  :: Double
  }

data Mine_score = Mine_score
  { n :: Int
  , m :: [Int]
  , mm:: [[Double]]
  }


{- exposed functions -}
--c'mine_compute_score :: Ptr C'Min_problem -> Ptr C'Mine_parameter -> IO (Ptr
--C'Mine_score)
mine_compute_score :: Mine_problem -> Mine_parameter -> Mine_score
mine_compute_score problem prameter = unsafePerformIO $ do
  let x' = map realToFrac (x problem) :: [CDouble]
      y' = map realToFrac (y problem) :: [CDouble]
      n' = fromIntegral (n problem)   :: CInt
      alpha' = realToFrac (alpha parameter) :: CDouble
      c' = realToFrac (c parameter) :: CDouble
  ptr'C'Mine_score <- withArray x' $ \ptr'x ->
    withArray y' $ \ptr'y -> do
      cproblem <- new (C'Mine_problem {n=n', x=ptr'x, y=ptr'y})
      cparameter <- new (C'Mine_parameter {alpha=alpha', c=c'})
      c'mine_compute_score cproblem cparameter
  aC'Mine_score <- peek ptr'C'Mine_score
  outm' <- peekArray (fromIntegral n') (m aC'Mine_score)
  let outm = map fromIntegral outm'
      outn = fromIntegral n'
      mm' <- peekArray outn (mm aC'Mine_score)
      mm'' = map (\i->peekArray (outm!!i) mm'!!i) [0..outn-1]
  return $ Mine_score {n = outn, m = outm, mm = mm''}


--c'mine_check_parameter :: Ptr C'Mine_parameter -> IO (CString)
mine_check_parameter :: Mine_parameter -> String
mine_check_parameter parameter = unsafePerformIO $ do
  let alpha' = realToFrac (alpha parameter) :: CDouble
      c' = realToFrac (c parameter) :: CDouble
  cparameter <- new (C'Mine_parameter {alpha=alpha', c=c'})
  aCString <- c'mine_check_parameter cparameter
  peekCString aCString


--c'mine_mic :: Ptr C'Mine_score -> IO (CDouble)
mine_mic :: Mine_score -> Double
mine_mic score = unsafePerformIO $ do
  let n' = fromIntegral (n score) :: CInt
      m' = map fromIntegral (m score) :: [CInt]
      mm'= map (\x -> map realToFrac) mm :: [[CDoouble]]
  out <- withArray n' m' $ \ptr'm -> do
    ptrPtr'mm <- sequence $ map (\x-> withArray n' x $ \ptr'mm -> return ptr'mm) mm'
    let aC'Mine_score = new (C'Mine_score{n=n', m=ptr'm, mm=ptrPtr'mm})
    c'mine_mic aC'Mine_score
  return $ realToFrac out


--c'mine_mas :: Ptr C'Mine_score -> IO (CDouble)
mine_mas :: Mine_score -> Double
mine_mas score = unsafePerformIO $ do
  let n' = fromIntegral (n score) :: CInt
      m' = map fromIntegral (m score) :: [CInt]
      mm'= map (\x -> map realToFrac) mm :: [[CDoouble]]
  out <- withArray n' m' $ \ptr'm -> do
    ptrPtr'mm <- sequence $ map (\x-> withArray n' x $ \ptr'mm -> return ptr'mm) mm'
    let aC'Mine_score = new (C'Mine_score{n=n', m=ptr'm, mm=ptrPtr'mm})
    c'mine_mas aC'Mine_score
  return $ realToFrac out


--c'mine_mev :: Ptr C'Mine_score -> IO (CDouble)
mine_mev :: Mine_score -> Double
mine_mev score = unsafePerformIO $ do
  let n' = fromIntegral (n score) :: CInt
      m' = map fromIntegral (m score) :: [CInt]
      mm'= map (\x -> map realToFrac) mm :: [[CDoouble]]
  out <- withArray n' m' $ \ptr'm -> do
    ptrPtr'mm <- sequence $ map (\x-> withArray n' x $ \ptr'mm -> return ptr'mm) mm'
    let aC'Mine_score = new (C'Mine_score{n=n', m=ptr'm, mm=ptrPtr'mm})
    c'mine_mev aC'Mine_score
  return $ realToFrac out


--c'mine_mcn :: Ptr C'Mine_score -> CDouble -> IO (CDouble)
mine_mcn :: Mine_score -> Double -> Double
mine_mcn score y = unsafePerformIO $ do
  let y' = realToFrac y :: CDouble
      n' = fromIntegral (n score) :: CInt
      m' = map fromIntegral (n score) :: [CInt]
      mm'= map (\x->map realToFrac) mm :: [[CDouble]]
  out <- withArray n' m' $ \ptr'm -> do
    ptrPtr'mm <- sequence $ map (\x->withArray n' x $ \ptr'mm -> return ptr'mm) mm'
    let aC'Mine_score = new (C'Mine_score{n=n', m=ptr'm, mm=ptrPtr'mm})
    c'mine_mcn aC'Mine_score y'
  return $ realToFrac out


--c'mine_mcn_general :: Ptr C'Mine_score -> IO (CDouble)
mine_mcn_general :: Mine_score -> Double
mine_mcn_general score = unsafePerformIO $ do
  let n' = fromIntegral (n score) :: CInt
      m' = map fromIntegral (m score) :: [CInt]
      mm'= map (\x -> map realToFrac) mm :: [[CDoouble]]
  out <- withArray n' m' $ \ptr'm -> do
    ptrPtr'mm <- sequence $ map (\x-> withArray n' x $ \ptr'mm -> return ptr'mm) mm'
    let aC'Mine_score = new(C'Mine_score{n=n', m=ptr'm, mm=ptrPtr'mm})
    c'mine_mcn_general aC'Mine_score
  return $ realToFrac out


--c'mine_free_score :: Ptr (Ptr C'Mine_score) -> IO()
-- Do we need this function? We use new to allocate memory for Mine_score
-- structure

instance Storable C'Mine_problem where
  sizeOf = const #size struct mine_problem
  alignment = sizeOf
  poke problem (C'Mine_problem val_n val_x val_y) = do
    (#poke struct mine_problem, n) problem val_n
    (#poke struct mine_problem, x) problem val_x
    (#poke struct mine_problem, y) problem val_y
  peek problem = do
    val_n <- (#peek struct mine_problem, n) problem
    val_x <- (#peek struct mine_problem, x) problem
    val_y <- (#peek struct mine_problem, y) problem
    return $ C'Mine_problem { n = val_n
                            , x = val_x
                            , y = val_y}


instance Storable C'Mine_parameter where
  sizeOf = const #size struct mine_parameter
  alignment = sizeOf
  poke parameter (C'Mine_parameter val_alpha val_c) = do
    (#poke struct mine_parameter, alpha) parameter val_alpha_
    (#poke struct mine_parameter, c) parameter val_c
  peek parameter = do
    val_alpha <- (#peek struct mine_parameter, alpha) parameter
    val_c     <- (#peek struct mine_parameter, c) parameter
    return $ C'Mine_parameter { alpha = val_alpha_
                              , c = val_c}


instance Storable C'Mine_score where
  sizeOf = const #size struct mine_score
  alignment = sizeOf
  poke score (C'Mine_score val_n val_m val_mm) = do
    (#poke struct mine_score, n) score val_n
    (#poke struct mine_score, m) score val_m
    (#poke struct mine_score, mm) score val_mm
  peek score = do
    val_n  <- (#peek struct mine_score, n) score
    val_m  <- (#peek struct mine_score, m) score
    val_mm <- (#peek struct mine_score, mm) score
    return $ C'Mine_score { n = val_n
                          , m = val_m
                          , mm=val mm}


foreign import ccall unsafe "mine.h mine_compute_score"
  c'mine_compute_score :: Ptr C'Min_problem -> Ptr C'Mine_parameter -> IO (Ptr C'Mine_score)
foreign import ccall unsafe "mine.h mine_check_parameter"
  c'mine_check_parameter :: Ptr C'Mine_parameter -> IO (Ptr CString)
foreign import ccall unsafe "mine.h mine_mic"
  c'mine_mic :: Ptr C'Mine_score -> IO (CDouble)
foreign import ccall unsafe "mine.h mine_mas"
  c'mine_mas :: Ptr C'Mine_score -> IO (CDouble)
foreign import ccall unsafe "mine.h mine_mev"
  c'mine_mev :: Ptr C'Mine_score -> IO (CDouble)
foreign import ccall unsafe "mine.h mine_mcn"
  c'mine_mcn :: Ptr C'Mine_score -> CDouble -> IO (CDouble)
foreign import ccall unsafe "mine.h mine_mcn_general"
  c'mine_mcn_general :: Ptr C'Mine_score -> IO (CDouble)
--foreign import ccall unsafe "mine.h mine_free_score"
--  c'mine_free_score :: Ptr (Ptr C'Mine_score) -> IO()




