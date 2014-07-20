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
    }

runSimulation :: Simulation -> [(Double, [Particle])]
runSimulation (Simulation _ initialSetup timeStep duration) = zip ([0, timeStep.. duration]) $ iterate (updateParticles timeStep) initialSetup

updateParticles :: TimeStep -> [Particle] -> [Particle]
updateParticles dt ps = ps
{-|
updateParticles dt ps = map updateParticle $ enactAllForces dt ps
    where
        updateParticle part@(Particle { pos = p, vel = v }) = part { pos = np }
            where
                np = p + dv
                    where
                        dv = v * Vector3D dt dt dt
|-}

-- Test simulations
testSimHydrogen :: Simulation
testSimHydrogen = Simulation "Single Hydrogen Atom" (spawnAtom Hydrogen (Vector3D 0 0 0) (Vector3D 0 0 0) []) 0.1 10
