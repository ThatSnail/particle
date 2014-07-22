module Simulation (
      Simulation ( Simulation, initialSetup, timeStep, simScale )
    , getSimResult
    , testSimHydrogen
    , testSimElectron
    , testSimElectronPositron
    , testSimTwoElectrons
    ) where

import System.Directory

import Particle
import Physics
import Forces
import Math
import Atom
import NumberTypes

data Simulation = Simulation {
      name :: String
    , prettyName :: String
    , initialSetup :: [Particle]
    , timeStep :: Float
    , duration :: Float
    , simScale :: Double
    } deriving Read

runSimulation :: Simulation -> [[Particle]]
runSimulation (Simulation _ _ initialSetup timeStep duration _) = take (floor (duration / timeStep)) $ iterate (updateParticles timeStep) initialSetup

updateParticles :: TimeStep -> [Particle] -> [Particle]
updateParticles dt ps = map updateParticle $ enactAllForces dt ps
    where
        updateParticle part@(Particle { pos = p, vel = v }) = part { pos = np }
            where
                np = p + dv
                    where
                        dv = v * Vector3D (realToFrac dt) (realToFrac dt) (realToFrac dt)

-- File operations (saving / loading simulations)
saveSimResult :: Simulation -> IO ()
saveSimResult sim@(Simulation {name = n}) = writeFile path (show $ runSimulation sim)
    where
        path = simulationPath sim

loadSimResult :: Simulation -> IO [[Particle]]
loadSimResult sim@(Simulation {name = n}) = readFile (n ++ ".sim") >>= return . read

simulationPath :: Simulation -> String
simulationPath Simulation { name = nameStr } = nameStr ++ ".sim"

-- Get the simulation result, either by running or loading a saved simulation
getSimResult :: Simulation -> IO [[Particle]]
getSimResult sim = do
    simAlreadyBuilt <- doesFileExist $ simulationPath sim
    if simAlreadyBuilt
        then loadSimResult sim
        else saveSimResult sim >> return (runSimulation sim)

-- Test simulations
testSimElectron :: Simulation
testSimElectron = Simulation "singleElectron" "Single Electron" [Particle Electron (Vector3D 0 0 0) (Vector3D 0 0 0)] 0.1 10 10

testSimTwoElectrons :: Simulation
testSimTwoElectrons = Simulation "twoElectrons" "Two Electrons" [Particle Electron (Vector3D (-0.5) 0 0) (Vector3D 0 0 0), Particle Electron (Vector3D 0.5 0 0) (Vector3D 0 0 0)] 0.001 1 10

testSimElectronPositron :: Simulation
testSimElectronPositron = Simulation "electronPositron" "Electron and Positron" [Particle Electron (Vector3D (-0.25) 0 0) (Vector3D 0 0 0), Particle Positron (Vector3D 0.25 0 0) (Vector3D 0 0 0)] 0.00001 1 10

testSimHydrogen :: Simulation
testSimHydrogen = Simulation "hydrogen" "Single Hydrogen Atom" (spawnAtom Hydrogen (Vector3D 0 0 0) (Vector3D 0 0 0) []) 0.001 1 10
