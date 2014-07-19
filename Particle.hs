module Particle (
      Particle ( Particle, pType, pos, vel )
    , ParticleType ( Proton, Neutron, Electron )
    , mass
    , charge )
    where

import Math

data ParticleType = Proton | Neutron | Electron deriving (Eq, Show)

data Particle = Particle {
      pType :: ParticleType
    , pos :: Vector3D Double
    , vel :: Vector3D Double
    } deriving Eq

-- All masses in kilograms
mass :: (Floating a) => ParticleType -> a
mass Proton = 1.67262 * 10 ** (-27)
mass Neutron = 1.67493 * 10 ** (-27)
mass Electron = 9.10939 * 10 ** (-31)

-- All charges in Couloumbs
charge :: (Floating a) => ParticleType -> a
charge Proton = 1.6 * 10 ** (-19)
charge Neutron = 0
charge Electron = (-1.6) * 10 ** (-19)
