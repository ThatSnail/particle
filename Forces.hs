{-| Elementary forces |-}

module Forces (
      enactAllForces
    )
    where

import Data.List
import Control.Applicative
import Math
import Particle
import Physics
import NumberTypes

data ForceType = EMForce

data Force = Force {
      fType :: ForceType
    , mag :: PreciseNum
    , dir :: Vector3D PreciseNum
    }

-- Simulate a new [Particle] with all forces applied throughout
-- TODO : Update with other forces
enactAllForces :: TimeStep -> [Particle] -> [Particle]
enactAllForces dt ps = enactForceType EMForce dt ps

-- Simulate a new [Particle] with force applied throughout
-- Assume linear development throughout timestep (Euler's approximation, 1 step)
enactForceType :: ForceType -> TimeStep -> [Particle] -> [Particle]
enactForceType EMForce dt ps = map applyAllForces ps
    where
        applyAllForces p = foldl (\p f -> applyForceSingle f dt p) p (forces p)
        forces p = map (force EMForce p) $ delete p ps

-- Force and potential calculations
force :: ForceType -> Particle -> Particle -> Force
force EMForce p1 p2 = Force EMForce (forceMag p1 p2) (forceDir p1 p2)
    where
        forceMag p1 p2 = realToFrac ((-coulombsConst) * (charge $ pType p1) * (charge $ pType p2)) / ((dist (pos p1) (pos p2)) ** 2)
        forceDir p1 p2 = norm $ (pos p2) - (pos p1)

-- Potential energy between two particles due to some force
potential :: ForceType -> Particle -> Particle -> Energy
potential EMForce p1 p2 = realToFrac ((-coulombsConst) * (charge $ pType p1) * (charge $ pType p2)) / (dist (pos p1) (pos p2))

-- Potential energy of the entire system
systemPotential :: [Particle] -> Energy
systemPotential ps = sum $ map (\(p1, p2) -> potential EMForce p1 p2) $ pairs ps
    where
        pairs = concatMap (zip . repeat . head <*> tail) . (take . length <*> iterate tail)

-- Apply a force onto one particle
applyForceSingle :: Force -> TimeStep -> Particle -> Particle
applyForceSingle (Force _ mag dir) dt p@(Particle {pType = pt, vel = v}) = p { vel = nvel }
    where
        nvel = v + (Vector3D da da da) * dir
            where
                m = mass pt
                a = mag / (realToFrac m)
                da = a * (realToFrac dt)
