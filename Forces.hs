{-| Elementary forces |-}

module Forces (
    )
    where

import Data.List
import Math
import Particle
import Physics

type TimeStep = Double
data ForceType = EMForce

data Force = Force {
      fType :: ForceType
    , mag :: Double
    , dir :: Vector3D Double
    }

-- Simulate a new [Particle] with force applied throughout
-- Assume linear development throughout timestep (Euler's approximation, 1 step)
enactForceType :: ForceType -> TimeStep -> [Particle] -> [Particle]
enactForceType EMForce dt ps = map applyAllForces ps
    where
        applyAllForces p = foldl (\p f -> applyForceSingle f dt p) p (forces p)
        forces p = map (force p) $ delete p ps
            where
                force p1 p2 = Force EMForce (forceMag p1 p2) (forceDir p1 p2)
                    where
                        forceMag p1 p2 = coulombsConst * (charge (pType p1)) * (charge (pType p2)) / (dist (pos p1) (pos p2))
                        forceDir p1 p2 = norm $ (pos p2) - (pos p1)

-- Apply a force onto one particle
applyForceSingle :: Force -> TimeStep -> Particle -> Particle
applyForceSingle (Force _ mag dir) dt p@(Particle pType pos vel) = Particle pType pos nvel
    where
        nvel = vel + (Vector3D da da da)
            where
                m = mass pType
                a = mag / m
                da = a * dt
