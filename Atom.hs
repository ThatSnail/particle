module Atom (
      Atom ( Hydrogen )
    , spawnAtom
    ) where

import System.Random
import Particle
import Physics
import Math

data Atom = Hydrogen | Helium

-- Spawns atom at position
spawnAtom :: Atom -> Vector3D Double -> Vector3D Double -> [Particle] -> [Particle]
spawnAtom Hydrogen pos vel ps = proton : electron : ps
    where
        proton = Particle Proton pos vel
        electron = Particle Electron (pos + Vector3D 0 bohrsRadius 0) vel
