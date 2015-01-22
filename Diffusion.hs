{-# LANGUAGE KindSignatures, TypeOperators, NoMonomorphismRestriction #-}


module Diffusion (diffusion, main, runDiffusionUntilConvergence) where
import Data.Vector
import qualified Data.Vector.Unboxed as V
import System.Environment   
import System.Directory  
import System.IO  
import Data.List  

main = do
  txt <- readFile "mydata.dat"
  let dat = Diffusion.conv txt
  print dat -- this prints out my chunk of data
  return ()

conv :: [Char] -> [[Double]]
conv x = Data.List.map (Data.List.map read . words) (lines x)

run1D :: [String] -> IO ()
run1D [fileName] = appendFile fileName ("testing" Data.List.++ "\n") 

dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("run1D", run1D)
            ]  

{- |
    This data allows us to differentiate between using boundary conditions or not during diffusion iterations on spatial grid. 
-}
data DiffusionProcess a = Middle | LeftBoundaryConstant a | RightBoundaryConstant a
                        deriving (Eq, Ord)

{- |
    Essentially the phase space and parameters.  List of boundary conditions, 'old' density space, 'new' density space, dt, and dx. 
-}
type DiffusionRuntime = ([DiffusionProcess Double], Vector Double, Vector Double, Double, Double) 

{- |
    Marches through spatial dimension and applies diffusion function recursively.
-}
runDiffusionProcess :: DiffusionRuntime -> DiffusionRuntime
runDiffusionProcess (LeftBoundaryConstant a :dps, old_cells, new_cells, dt, dx) = runDiffusionProcess (dps, old_cells,                    new_cells Data.Vector.++ diffuse(LeftBoundaryConstant a,  Data.Vector.take 2 old_cells, dt, dx), dt, dx)
runDiffusionProcess (Middle:dps,                  old_cells, new_cells, dt, dx) = runDiffusionProcess (dps, Data.Vector.drop 1 old_cells, new_cells Data.Vector.++ diffuse(Middle,                  Data.Vector.take 3 old_cells, dt, dx), dt, dx)
runDiffusionProcess ([RightBoundaryConstant a],   old_cells, new_cells, dt, dx) =                     ([],  old_cells,                    new_cells Data.Vector.++ diffuse(RightBoundaryConstant a, Data.Vector.take 2 old_cells, dt, dx), dt, dx)

{- |
    Computes new density value for next time step for a cell based on adjacent cells in current time step. 
-}
diffuse :: (DiffusionProcess Double, Vector Double, Double, Double) -> Vector Double
diffuse (LeftBoundaryConstant a,  cells, dt, dx)  = fromList ([(cells ! 0) + (dt/(dx*dx))*((cells ! 1) - 2.0*(cells ! 0) + (a))])                                          
diffuse (Middle,                  cells, dt, dx)  = fromList ([(cells ! 1) + (dt/(dx*dx))*((cells ! 2) - 2.0*(cells ! 1) + (cells ! 0))])
diffuse (RightBoundaryConstant a, cells, dt, dx)  = fromList ([(cells ! 1) + (dt/(dx*dx))*((a)         - 2.0*(cells ! 1) + (cells ! 0))])

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

initialRuntime2 :: DiffusionRuntime
initialRuntime2 = (diffusion_steps, Data.Vector.replicate 20 1.0, fromList [], 2.0, 2.0)


{- |
    Describe our spatial geometry using a list.
-}
diffusion_steps :: [DiffusionProcess Double]
diffusion_steps = [LeftBoundaryConstant 5.0] Prelude.++ Prelude.replicate 18 Middle Prelude.++ [RightBoundaryConstant 0.0]

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
instance Show (DiffusionProcess a) where
  --show (LeftBoundary) = "LeftBoundary"
  show (Middle) = "Middle"
  --show (RightBoundary) = "RightBoundary"
  show (LeftBoundaryConstant a) = "LeftBoundaryConstant"
  show (RightBoundaryConstant a) = "RightBoundaryConstant"

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
--main = do
--    (command:args) <- getArgs
--    let (Just action) = lookup command dispatch
--    action args
--    content <- readFile "input.txt"
--    print $ lines $ content
{- |
Next thing to do is solve implicit using matrix solve via GS see: http://www3.nd.edu/~jjwteach/441/PdfNotes/lecture16.pdf
https://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm#Python
http://www.haskell.org/haskellwiki/Numeric_Haskell:_A_Vector_Tutorial#Array_Types
-}

--As an inverse of show, need to impl read for the various data types to be able to read them from text file
