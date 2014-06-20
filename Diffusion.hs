{-# LANGUAGE KindSignatures, TypeOperators, NoMonomorphismRestriction #-}


module Diffusion (diffusion, stepIo) where
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.IO.BMP as RIOB
import qualified Data.Array.Repa.IO.Matrix as RIOM
import Data.List 

{- |
     This contains blah blah blah

     >>> diffusion
     ()
-}
diffusion :: ()
diffusion = ()

-- point
type DIM0 = R.DIM0
-- vector
type DIM1 = R.DIM1

--space
nx :: Int
nx = 10

lx :: Double
lx = 1.0

dx :: Double
dx = lx /  fromIntegral nx

--time
nt :: Int
nt = 10

lt :: Double
lt = 1.0

dt :: Double
dt = lt /  fromIntegral nt

cj :: R.Array R.U DIM1 Double
cj = R.fromListUnboxed (R.Z R.:. (nx::Int)) [dx,dx+dx..lx]

cjp :: R.Array R.D DIM1 Double
cjp = R.map (+1) cj


runOneTimeStep ::  R.Array R.U DIM1 Double -> R.Array R.D DIM1 Double
runOneTimeStep c = R.map (+1) c

stepIo ::R.Array R.U DIM1 Double -> IO (R.Array R.U DIM1 Double)
stepIo c = R.computeP (runOneTimeStep c) :: IO (R.Array R.U R.DIM1 Double)


main :: IO ()
main = do
  print cj
  z <- stepIo cj
  print z
  z2 <- stepIo z
  print z2
