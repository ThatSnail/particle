import Graphics.Gloss
import Text.Printf

import GHC.Float
import Simulation
import Particle
import Math
import Physics
import NumberTypes

screenSize :: (Num a) => a
screenSize = 500

screenTiles :: (Num a) => a
screenTiles = 4

particleSizeFactor :: Float
--particleSizeFactor = 1000000000
particleSizeFactor = 100000000

-- Set how much time passes in one second during the animation
timeScale :: Float
timeScale = 0.01

buildAnimation :: Simulation -> [[Particle]] -> Float -> Picture
buildAnimation sim@(Simulation {simScale = ss, timeStep = ts}) frames t = pictures $ [drawParticles ss (frames !! (floor (t * timeScale / ts))), drawGUI sim (t * timeScale)]

drawGUI :: Simulation -> Float -> Picture
drawGUI sim@(Simulation {prettyName = n, duration = dur, simScale = ss}) time = pictures $ concat [picsGrid, picsText]
    where
        picsText = map (translate picsTextX picsTextY . color green . scale 0.1 0.1) [picName, picTime, picDuration, picScale]
        picsTextX = -(fromIntegral screenSize / 2) + 20
        picsTextY = (fromIntegral screenSize / 2) - 20
        picName = text n
        picTime = translate 0 (-200) $ text ("Time: " ++ printf "%.3f\n" time ++ "s")
        picDuration = translate 0 (-400) $ text ("Duration: " ++ show dur ++ "s")
        picScale = translate 0 (-600) $ text ("Scale: " ++ show ss ++ "m")
        picsGrid = map (color (greyN 0.1)) $ lines
            where
                lines = horizLines ++ vertLines
                    where
                        hlss = screenSize / 2
                        horizLines = map (\x -> line [(x, (-hlss)), (x, hlss)]) [(-hlss),((-hlss) + (hlss / screenTiles)).. hlss]
                        vertLines = map (\y -> line [((-hlss), y), (hlss, y)]) [(-hlss), ((-hlss) + (hlss / screenTiles)).. hlss]

drawParticles :: Float -> [Particle] -> Picture
drawParticles simScale ps = pictures $ map (drawParticle simScale) ps

drawParticle :: Float -> Particle -> Picture
drawParticle simScale (Particle pType (Vector3D x y z) _) = translate (realToFrac nx) (realToFrac ny) $ color col $ circleSolid $ max 1 (realToFrac nr)
    where
        nx = (realToFrac x) * (fromIntegral (screenSize `div` 2)) / (simScale * screenTiles)
        ny = (realToFrac y) * (fromIntegral (screenSize `div` 2)) / (simScale * screenTiles)
        nr = fromIntegral screenSize * particleSizeFactor * (realToFrac (radius pType)) / simScale
        col
            | pType == Proton = red
            | pType == Neutron = greyN 0.5
            | pType == Electron = yellow
            | pType == Positron = blue
            | pType == MacroPositron = blue

drawSimulation :: Simulation -> IO ()
drawSimulation simOut = getSimResult simOut >>= \frames -> animate (InWindow "Particle Simulator" (screenSize, screenSize) (10, 10)) black (buildAnimation simOut frames)

main :: IO ()
main = drawSimulation $ testSimElectronPositron
