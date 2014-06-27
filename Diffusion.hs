{-# LANGUAGE KindSignatures, TypeOperators, NoMonomorphismRestriction #-}


module Diffusion (diffusion) where
import Data.Vector
import qualified Data.Vector.Unboxed as V

a :: Vector Integer
a = fromList [10,20,30,40]

b :: Vector Integer
b = Data.Vector.replicate 10 2

c :: Vector Int
c = generate 10 (^2)

two_dim :: Vector (Vector Int)
two_dim = generate 10 (\n -> Data.Vector.replicate 10 n)


size :: Int
size = 10

data DiffusionProcess = LeftBoundary | Middle | RightBoundary

diffusion_steps :: [DiffusionProcess]
diffusion_steps = [LeftBoundary] Prelude.++ Prelude.replicate size Middle Prelude.++ [RightBoundary]

type DiffusionRuntime = ([DiffusionProcess], Vector Double, Int) 

runDiffusionProcess :: DiffusionRuntime -> DiffusionRuntime
runDiffusionProcess (LeftBoundary:dps, cells, s) = runDiffusionProcess (dps, diffuse (LeftBoundary, cells,s), s)
runDiffusionProcess (Middle:dps, cells, s) = runDiffusionProcess (dps, diffuse (Middle, cells,s), s)
runDiffusionProcess ([RightBoundary],cells,s) = ([], diffuse (RightBoundary, cells,s), s)

diffuse :: (DiffusionProcess, Vector Double, Int) -> Vector Double
diffuse (LeftBoundary, cells, s) = cells
diffuse (Middle, cells, s) = cells
diffuse (RightBoundary, cells, s) = cells

instance Show DiffusionProcess where
  show (LeftBoundary) = "LeftBoundary"
  show (Middle) = "Middle"
  show (RightBoundary) = "RightBoundary"


--diffuse :: Vector Double -> Vector Double


{- |
     This contains blah blah blah

     >>> diffusion
     ()
-}
diffusion :: ()
diffusion = ()
