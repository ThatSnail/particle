module Particle (
      Particle ( Particle, pType, pos, vel )
    , ParticleType ( Proton, Neutron, Electron, Positron, MacroPositron )
    , mass
    , charge
    , radius
    , kinetic
    , changeKinetic
    ) where

import Math
import Physics
import NumberTypes

data ParticleType = Proton | Neutron | Electron | Positron | MacroPositron deriving (Eq, Show, Read)

data Particle = Particle {
      pType :: ParticleType
    , pos :: Vector3D PreciseNum
    , vel :: Vector3D PreciseNum
    } deriving (Eq, Show, Read)

-- All masses in kilograms
mass :: ParticleType -> Double
mass Proton = 1.67262 / (10 ** 27)
mass Neutron = 1.67493 / (10 ** 27)
mass Electron = 9.10939 / (10 ** 31)
mass Positron = 9.10939 / (10 ** 31)
mass MacroPositron = 10 ** 100

-- All charges in Couloumbs
charge :: ParticleType -> Double
charge Proton = 1.6 / (10 ** 19)
charge Neutron = 0
charge Electron = (-1.6) / (10 ** 19)
charge Positron = 1.6 / (10 ** 19)
charge MacroPositron = 1.6 / (10 ** 19)

-- All radii in meters
-- NOTE: ALL OF THESE ARE VERY, VERY WRONG!
radius :: ParticleType -> Double
radius Proton = 5.29 / (10 ** 11)
radius Neutron = 5.29 / (10 ** 11)
radius Electron = 5.29 / (10 ** 11)
radius Positron = 5.29 / (10 ** 11)
radius MacroPositron = 5.29 / (10 ** 11)

-- Kinetic energy of particle
kinetic :: Particle -> Energy
kinetic p = 0.5 * (mass $ pType p) * (mag $ vel p) ** 2

-- Change the energy of the particle by some amount by changing velocity
changeKinetic :: Particle -> Energy -> Particle
changeKinetic p@(Particle {vel = v}) de = p { vel = nvel }
    where
        nvel = changeMag v $ (sqrt ((mag v) ** 2 + 2 * de / mass (pType p))) - (mag v)
