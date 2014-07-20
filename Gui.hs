import Graphics.Gloss

import GHC.Float
import Simulation
import Particle
import Math
import Algorithm

w :: Int
w = 500

h :: Int
h = 500

buildAnimation :: [(Double, [Particle])] -> Float -> Picture
buildAnimation frames t = getPicture maybeInd
    where
        ts = map fst frames
        ps = map snd frames
        maybeInd = binarySearchLower (float2Double t) ts
        getPicture (Just i) = drawParticles (ps !! i)
        getPicture Nothing = blank

drawParticles :: [Particle] -> Picture
drawParticles ps = pictures $ map drawParticle ps

drawParticle :: Particle -> Picture
drawParticle p@(Particle pType (Vector3D x y z) _) = translate (double2Float x) (double2Float y) $ color red $ circleSolid 10

drawSimulation :: [(Double, [Particle])] -> IO ()
drawSimulation simOut = animate (InWindow "Particle Simulator" (w, h) (10, 10)) black $ buildAnimation simOut

main :: IO ()
main = drawSimulation $ runSimulation $ testSimHydrogen
