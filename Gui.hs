import Graphics.Gloss

import GHC.Float
import Simulation
import Particle
import Math
import Physics
import NumberTypes

screenSize :: Int
screenSize = 500

particleSizeFactor :: Double
particleSizeFactor = 0.4

-- Set how much time passes in one second during the animation
timeScale :: Float
timeScale = 0.1

buildAnimation :: Simulation -> [[Particle]] -> Float -> Picture
buildAnimation (Simulation {simScale = ss, timeStep = ts}) frames t = drawParticles ss (frames !! (floor (t * timeScale / ts)))

drawParticles :: Double -> [Particle] -> Picture
drawParticles simScale ps = pictures $ map (drawParticle simScale) ps

drawParticle :: Double -> Particle -> Picture
drawParticle simScale (Particle pType (Vector3D x y z) _) = translate (realToFrac nx) (realToFrac ny) $ color col $ circleSolid $ max 1 (realToFrac nr)
    where
        nx = (realToFrac x) * (fromIntegral (screenSize `div` 2)) / simScale
        ny = (realToFrac y) * (fromIntegral (screenSize `div` 2)) / simScale
        nr = fromIntegral screenSize * particleSizeFactor * (realToFrac (radius pType)) / simScale
        col
            | pType == Proton = red
            | pType == Neutron = greyN 0.5
            | pType == Electron = yellow
            | pType == Positron = blue

drawSimulation :: Simulation -> IO ()
drawSimulation simOut = getSimResult simOut >>= \frames -> animate (InWindow "Particle Simulator" (screenSize, screenSize) (10, 10)) black (buildAnimation simOut frames)

main :: IO ()
main = drawSimulation $ testSimElectron
