import Graphics.Gloss

import GHC.Float
import Simulation
import Particle
import Math
import Algorithm
import Physics

screenSize :: Int
screenSize = 500

realSize :: Double
realSize = bohrsRadius * 100

particleSizeFactor :: (Num a) => a
particleSizeFactor = 1

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
drawParticle p@(Particle pType (Vector3D x y z) _) = translate (double2Float nx) (double2Float ny) $ color col $ circleSolid nr
    where
        nx = x * (fromIntegral (screenSize `div` 2)) / realSize
        ny = y * (fromIntegral (screenSize `div` 2)) / realSize
        nr = double2Float $ fromIntegral screenSize * particleSizeFactor * (radius pType) / realSize
        col
            | pType == Proton = red
            | pType == Neutron = greyN 0.5
            | pType == Electron = yellow

drawSimulation :: [(Double, [Particle])] -> IO ()
drawSimulation simOut = animate (InWindow "Particle Simulator" (screenSize, screenSize) (10, 10)) black $ buildAnimation simOut

main :: IO ()
main = drawSimulation $ runSimulation $ testSimHydrogen
