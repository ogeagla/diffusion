{-# LANGUAGE KindSignatures, TypeOperators, NoMonomorphismRestriction #-}


module Diffusion (diffusion) where
import Data.Vector
import qualified Data.Vector.Unboxed as V

--
-- examples
a :: Vector Integer
a = fromList [10,20,30,40]

b :: Vector Integer
b = Data.Vector.replicate 10 2

c :: Vector Int
c = generate 10 (^2)

two_dim :: Vector (Vector Int)
two_dim = generate 10 (\n -> Data.Vector.replicate 10 n)
-- /examples
--

{- |
    This data allows us to differentiate between using boundary conditions or not during diffusion iterations on spatial grid. 
-}
data DiffusionProcess = LeftBoundary | Middle | RightBoundary

{- |
    Essentially the phase space and parameters.  List of boundary conditions, 'old' density space, 'new' density space, dt, and dx. 
-}
type DiffusionRuntime = ([DiffusionProcess], Vector Double, Vector Double, Double, Double) 

{- |
    Marches through spatial dimension and applies diffusion function recursively.
-}
runDiffusionProcess :: DiffusionRuntime -> DiffusionRuntime
runDiffusionProcess (LeftBoundary:dps, old_cells, new_cells, dt, dx) = runDiffusionProcess (dps, old_cells,                    new_cells Data.Vector.++ diffuse(LeftBoundary,  Data.Vector.take 2 old_cells, dt, dx), dt, dx)
runDiffusionProcess (Middle:dps,       old_cells, new_cells, dt, dx) = runDiffusionProcess (dps, Data.Vector.drop 1 old_cells, new_cells Data.Vector.++ diffuse(Middle,        Data.Vector.take 3 old_cells, dt, dx), dt, dx)
runDiffusionProcess ([RightBoundary],  old_cells, new_cells, dt, dx) =                     ([],  old_cells,                    new_cells Data.Vector.++ diffuse(RightBoundary, Data.Vector.take 2 old_cells, dt, dx), dt, dx)

{- |
    Computes new density value for next time step for a cell based on adjacent cells in current time step. 
-}
diffuse :: (DiffusionProcess, Vector Double, Double, Double) -> Vector Double
diffuse (LeftBoundary,  cells, dt, dx)  = fromList ([(cells ! 0) + (dt/(dx*dx))*((cells ! 1) - 2.0*(cells ! 0) + (5.0))])                                          
diffuse (Middle,        cells, dt, dx)  = fromList ([(cells ! 1) + (dt/(dx*dx))*((cells ! 2) - 2.0*(cells ! 1) + (cells ! 0))])
diffuse (RightBoundary, cells, dt, dx)  = fromList ([(cells ! 1) + (dt/(dx*dx))*((0.0) - 2.0*(cells ! 1) + (cells ! 0))])

{- |
    Sum square of a Vector.
-}
sumSqr :: Vector Double -> Double
sumSqr v = Data.Vector.sum $ Data.Vector.zipWith (*) v v


{- |
    Arbitrary convergence function, which compares magnitude of difference vector to threshold.
-}
isConverged :: Vector Double -> Vector Double -> Double -> Bool
isConverged v1 v2 thresh =  sqrt (sumSqr (Data.Vector.zipWith (-) v2 v1)) < thresh
  --sqrt (Data.Vector.sum $ Data.Vector.zipWith (*) v1  v2)  > thresh

{- |
    The initial phase space.
-}
initialRuntime :: DiffusionRuntime
initialRuntime = (diffusion_steps, Data.Vector.replicate 20 1.0, fromList [], 0.1, 1.0)

{- |
    Describe our spatial geometry using a list.
-}
diffusion_steps :: [DiffusionProcess]
diffusion_steps = [LeftBoundary] Prelude.++ Prelude.replicate 18 Middle Prelude.++ [RightBoundary]

{- |
    This is the iterative diffusion process which first checks for convergence otherwise recurses.
-}
iterateDiffusion :: DiffusionRuntime -> DiffusionRuntime -> Double -> Int-> (Int, Vector Double) 
iterateDiffusion (dps1, old1, new1, dt1, dx1) (dps2, old2, new2, dt2, dx2) thresh iter
  | isConverged old1 new2 thresh  = (iter, new2)
  | otherwise = iterateDiffusion (dps1, new2, fromList [], dt1, dx1) (runDiffusionProcess (dps1, new2, fromList [], dt1, dx1)) thresh (iter + 1)


{- |
    Arbitrary convergence threshold.
-}
convergence_threshold :: Double
convergence_threshold = 0.0005

{- |
    This is currently being used to run the iterations and print out the results.
    I want to avoid putting logic here and not have to call the first iteration explicitly, but for simplicity I have not yet refactored this out.
-}
runDiffusionUntilConvergence :: IO ()
runDiffusionUntilConvergence = do print ("init runtime: ", initialRuntime)
                                  print (iterateDiffusion initialRuntime (runDiffusionProcess initialRuntime) convergence_threshold 1)

{- |
    How to display DiffusionProcess.  Although DiffusionProcess could have just 'derived' Show, I just did this explicitly instead.
-}
instance Show DiffusionProcess where
  show (LeftBoundary) = "LeftBoundary"
  show (Middle) = "Middle"
  show (RightBoundary) = "RightBoundary"


--diffuse :: Vector Double -> Vector Double


{- |
     This contains the diffusion module which is still empty.  Spec TBD.

     >>> diffusion
     ()
-}
diffusion :: ()
diffusion = ()

{- |
    Main function which will allow module to compile and run as executable

    >>> main
    IO ()
-}
main :: IO ()
main = do 
  putStrLn "The result is: "
