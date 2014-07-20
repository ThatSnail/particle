module Simulation (
      runSimulation
    , testSimHydrogen
    ) where

import Particle
import Physics
import Forces
import Math
import Atom

data Simulation = Simulation {
      name :: String
    , initialSetup :: [Particle]
    , timeStep :: Double
    , duration :: Double
    , simScale :: Double
    }

runSimulation :: Simulation -> [(Double, [Particle])]
runSimulation (Simulation _ initialSetup timeStep duration _) = zip ([0, timeStep.. duration]) $ iterate (updateParticles timeStep) initialSetup

updateParticles :: TimeStep -> [Particle] -> [Particle]
updateParticles dt ps = map updateParticle $ enactAllForces dt ps
    where
        updateParticle part@(Particle { pos = p, vel = v }) = part { pos = np }
            where
                np = p + dv
                    where
                        dv = v * Vector3D dt dt dt

-- Test simulations
testSimElectron :: Simulation
testSimElectron = Simulation "Single Electron" [Particle Electron (Vector3D 0 0 0) (Vector3D 0 0 0)] 0.1 10 (bohrsRadius * 100)

testSimTwoElectrons :: Simulation
testSimTwoElectrons = Simulation "Two Electrons" [Particle Electron (Vector3D (-0.5) 0 0) (Vector3D 0 0 0), Particle Electron (Vector3D 0.5 0 0) (Vector3D 0 0 0)] 0.001 10 10

testSimHydrogen :: Simulation
testSimHydrogen = Simulation "Single Hydrogen Atom" (spawnAtom Hydrogen (Vector3D 0 0 0) (Vector3D 0 0 0) []) 0.1 10 (bohrsRadius * 100)
