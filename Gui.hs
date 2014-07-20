import Graphics.Gloss

import GHC.Float
import Simulation
import Particle
import Math
import Algorithm
import Physics

screenSize :: Int
screenSize = 500

particleSizeFactor :: Double
particleSizeFactor = 0.4

-- Set how much time passes in one second during the animation
timeScale :: Float
timeScale = 0.1

buildAnimation :: Simulation -> Float -> Picture
buildAnimation sim@(Simulation {simScale = ss}) t = getPicture maybeInd
    where
        frames = runSimulation sim
        ts = map fst frames
        ps = map snd frames
        maybeInd = binarySearchLower (float2Double (t * timeScale)) ts
        getPicture (Just i) = drawParticles ss (ps !! i)
        getPicture Nothing = blank

drawParticles :: Double -> [Particle] -> Picture
drawParticles simScale ps = pictures $ map (drawParticle simScale) ps

drawParticle :: Double -> Particle -> Picture
drawParticle simScale p@(Particle pType (Vector3D x y z) _) = translate (double2Float nx) (double2Float ny) $ color col $ circleSolid $ max 1 nr
    where
        nx = x * (fromIntegral (screenSize `div` 2)) / simScale
        ny = y * (fromIntegral (screenSize `div` 2)) / simScale
        nr = double2Float $ fromIntegral screenSize * particleSizeFactor * (radius pType) / simScale
        col
            | pType == Proton = red
            | pType == Neutron = greyN 0.5
            | pType == Electron = yellow
            | pType == Positron = blue

drawSimulation :: Simulation -> IO ()
drawSimulation simOut = animate (InWindow "Particle Simulator" (screenSize, screenSize) (10, 10)) black $ buildAnimation simOut

main :: IO ()
main = drawSimulation $ testSimElectronPositron
