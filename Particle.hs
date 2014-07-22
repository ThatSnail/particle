module Particle (
      Particle ( Particle, pType, pos, vel )
    , ParticleType ( Proton, Neutron, Electron, Positron )
    , mass
    , charge
    , radius
    ) where

import Math
import Physics
import NumberTypes

data ParticleType = Proton | Neutron | Electron | Positron deriving (Eq, Show, Read)

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

-- All charges in Couloumbs
charge :: ParticleType -> Double
charge Proton = 1.6 / (10 ** 19)
charge Neutron = 0
charge Electron = (-1.6) / (10 ** 19)
charge Positron = 1.6 / (10 ** 19)

-- All radii in meters
-- NOTE: ALL OF THESE ARE VERY, VERY WRONG!
radius :: ParticleType -> Double
radius Proton = 5.29 / (10 ** 11)
radius Neutron = 5.29 / (10 ** 11)
radius Electron = 5.29 / (10 ** 11)
radius Positron = 5.29 / (10 ** 11)
