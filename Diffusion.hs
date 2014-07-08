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


cells_init :: Vector Double
cells_init = Data.Vector.replicate size 1.0

size :: Int
size = 2

data DiffusionProcess = LeftBoundary | Middle | RightBoundary

diffusion_steps :: [DiffusionProcess]
diffusion_steps = [LeftBoundary] Prelude.++ Prelude.replicate size Middle Prelude.++ [RightBoundary]

type DiffusionRuntime = ([DiffusionProcess], Vector Double, Vector Double, Int) 

runDiffusionProcess :: DiffusionRuntime -> DiffusionRuntime
runDiffusionProcess (LeftBoundary:dps, old_cells, new_cells, s) = runDiffusionProcess (dps, old_cells,                    new_cells Data.Vector.++ diffuse (LeftBoundary,  Data.Vector.take 2 old_cells,s), s)
runDiffusionProcess (Middle:dps,       old_cells, new_cells, s) = runDiffusionProcess (dps, Data.Vector.drop 1 old_cells, new_cells Data.Vector.++ diffuse (Middle,        Data.Vector.take 3 old_cells,s), s)
runDiffusionProcess ([RightBoundary],  old_cells, new_cells, s) =                     ([],  old_cells,                    new_cells Data.Vector.++ diffuse (RightBoundary, Data.Vector.take 2 old_cells,s), s)

diffuse :: (DiffusionProcess, Vector Double, Int) -> Vector Double
diffuse (LeftBoundary,  cells, s)  = fromList[(Data.Vector.foldl (+) 0.0 cells) / 3.0]::Vector Double
diffuse (Middle,        cells, s)  = fromList[(Data.Vector.foldl (+) 0.0 cells) / 3.0]::Vector Double
diffuse (RightBoundary, cells, s)  = fromList[(Data.Vector.foldl (+) 0.0 cells) / 3.0]::Vector Double

sumSqr :: Vector Double -> Double
sumSqr v = Data.Vector.sum $ Data.Vector.zipWith (*) v v



isConverged :: Vector Double -> Vector Double -> Double -> Bool
isConverged v1 v2 thresh =  sqrt (sumSqr (Data.Vector.zipWith (-) v2 v1)) < thresh
  --sqrt (Data.Vector.sum $ Data.Vector.zipWith (*) v1  v2)  > thresh

initialRuntime :: DiffusionRuntime
initialRuntime = (diffusion_steps, fromList [1.0,2.0,3.0,4.0], fromList [], 4)

runUntilConverged :: DiffusionRuntime -> DiffusionRuntime -> Double -> Int-> (Int, Vector Double) 
runUntilConverged (dps1, old1, new1, size1) (dps2, old2, new2, size2) thresh iter
  | isConverged old1 new2 thresh  = (iter, new2)
  | otherwise = runUntilConverged (dps1, new2, fromList [], size2) (runDiffusionProcess (dps1, new2, fromList [], size2)) thresh (iter + 1)


runDiffusionUntilConvergence :: IO ()
runDiffusionUntilConvergence = do print initialRuntime
                                  print (runUntilConverged initialRuntime (runDiffusionProcess initialRuntime) 0.001 0)

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

main :: IO ()
main = do 
  putStrLn "The result is: "
