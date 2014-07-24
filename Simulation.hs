{-|
module Simulation (
      Simulation ( Simulation, prettyName, initialSetup, timeStep, duration, simScale )
    , getSimResult
    , testSimHydrogen
    , testSimElectron
    , testSimElectronPositron
    , testSimElectronMacroPositron
    , testSimTwoElectrons
    ) where
|-}

import System.Directory
import Control.Applicative

import Particle
import Physics
import Forces
import Math
import Atom
import NumberTypes

data Simulation = Simulation {
      name :: String -- Simulation name (used for .sim files)
    , prettyName :: String -- Pretty name
    , initialSetup :: [Particle] -- First Particle state (t = 0)
    , timeStep :: Float -- dt to use for calculations (lower = more precision)
    , duration :: Float -- length of time to run simulation for
    , simScale :: Float -- width of one square (in meters)
    } deriving Read

-- Compute Hamiltonian constraint
hamiltonian :: [Particle] -> Energy
hamiltonian ps = systemKinetic ps + systemPotential ps

-- Hamiltonian difference between two states of a system
hamiltonianErr :: [Particle] -> [Particle] -> Energy
hamiltonianErr ps1 ps2 = abs ((hamiltonian ps1) - (hamiltonian ps2))

-- Kinetic energy of the entire system
systemKinetic :: [Particle] -> Energy
systemKinetic ps = sum $ map kinetic ps

-- Potential energy of the entire system
systemPotential :: [Particle] -> Energy
systemPotential ps = sum $ map (\(p1, p2) -> potential EMForce p1 p2) $ pairs ps
    where
        pairs = concatMap (zip . repeat . head <*> tail) . (take . length <*> iterate tail)

runSimulation :: Simulation -> [[Particle]]
runSimulation (Simulation _ _ initialSetup timeStep duration _) = take (floor (duration / timeStep)) $ iterate (updateParticles timeStep) initialSetup

updateParticles :: TimeStep -> [Particle] -> [Particle]
updateParticles dt ps = ps'
    where
        updatedps = zipWith resetVel (map updateParticle (enactAllForces dt ps)) ps
        forcedps = enactAllForces dt updatedps
        kineticChanges = zipWith (\ps1 ps2 -> (kinetic ps1) - (kinetic ps2)) forcedps ps
        a = ((hamiltonian ps) - (systemPotential updatedps)) / (systemKinetic forcedps)
        scaledKineticChanges = map (*a) kineticChanges
        ps' = zipWith changeKinetic updatedps scaledKineticChanges
        updateParticle part@(Particle { pos = p, vel = v }) = part { pos = np }
            where
                np = p + dv
                    where
                        dv = v * Vector3D (realToFrac dt) (realToFrac dt) (realToFrac dt)
        resetVel p1 p2@(Particle { vel = v }) = p1 { vel = v }

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
testSimElectron = Simulation {
      name = "singleElectron"
    , prettyName = "Single Electron"
    , initialSetup = [Particle Electron (Vector3D 0 0 0) (Vector3D 0 0 0)]
    , timeStep = 0.1
    , duration = 10
    , simScale = 10
    }

testSimTwoElectrons :: Simulation
testSimTwoElectrons = Simulation {
      name = "twoElectrons"
    , prettyName = "Two Electrons"
    , initialSetup = [Particle Electron (Vector3D (-0.5) 0 0) (Vector3D 0 0 0), Particle Electron (Vector3D 0.5 0 0) (Vector3D 0 0 0)]
    , timeStep = 0.001
    , duration = 1
    , simScale = 10
    }

testSimElectronPositron :: Simulation
testSimElectronPositron = Simulation {
      name = "electronPositron"
    , prettyName = "Electron and Positron"
    , initialSetup = [Particle Electron (Vector3D (-0.25) 0 0) (Vector3D 0 0 0), Particle Positron (Vector3D 0.25 0 0) (Vector3D 0 0 0)]
    , timeStep = 0.001
    , duration = 1
    , simScale = 0.25
    }

testSimElectronMacroPositron :: Simulation
testSimElectronMacroPositron = Simulation {
      name = "electronMacroPositron"
    , prettyName = "Electron and Macro Positron"
    , initialSetup = [Particle Electron (Vector3D (-0.25) 0 0) (Vector3D 0 0 0), Particle MacroPositron (Vector3D 0 0 0) (Vector3D 0 0 0)]
    , timeStep = 0.001
    , duration = 1
    , simScale = 0.25
    }

testSimHydrogen :: Simulation
testSimHydrogen = Simulation {
      name = "hydrogen"
    , prettyName = "Single Hydrogen Atom"
    , initialSetup = spawnAtom Hydrogen (Vector3D 0 0 0) (Vector3D 0 0 0) []
    , timeStep = 0.001
    , duration = 1
    , simScale = 10
    }
