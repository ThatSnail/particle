module Simulation (
      Simulation ( Simulation, timeStep, simScale )
    , runSimulation
    , testSimHydrogen
    , testSimElectron
    , testSimElectronPositron
    , testSimTwoElectrons
    ) where

import Particle
import Physics
import Forces
import Math
import Atom
import NumberTypes

data Simulation = Simulation {
      name :: String
    , initialSetup :: [Particle]
    , timeStep :: Float
    , duration :: Float
    , simScale :: Double
    }

runSimulation :: Simulation -> [[Particle]]
runSimulation (Simulation _ initialSetup timeStep duration _) = iterate (updateParticles timeStep) initialSetup

updateParticles :: TimeStep -> [Particle] -> [Particle]
updateParticles dt ps = map updateParticle $ enactAllForces dt ps
    where
        updateParticle part@(Particle { pos = p, vel = v }) = part { pos = np }
            where
                np = p + dv
                    where
                        dv = v * Vector3D (realToFrac dt) (realToFrac dt) (realToFrac dt)

-- Test simulations
testSimElectron :: Simulation
testSimElectron = Simulation "Single Electron" [Particle Electron (Vector3D 0 0 0) (Vector3D 0 0 0)] 0.001 10 10

testSimTwoElectrons :: Simulation
testSimTwoElectrons = Simulation "Two Electrons" [Particle Electron (Vector3D (-0.5) 0 0) (Vector3D 0 0 0), Particle Electron (Vector3D 0.5 0 0) (Vector3D 0 0 0)] 0.001 10 10

testSimElectronPositron :: Simulation
testSimElectronPositron = Simulation "Electron and Positron" [Particle Electron (Vector3D (-0.5) 0 0) (Vector3D 0 0 0), Particle Positron (Vector3D 0.5 0 0) (Vector3D 0 0 0)] 0.001 10 10

testSimHydrogen :: Simulation
testSimHydrogen = Simulation "Single Hydrogen Atom" (spawnAtom Hydrogen (Vector3D 0 0 0) (Vector3D 0 0 0) []) 0.001 10 10
