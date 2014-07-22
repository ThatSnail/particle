module Atom (
      Atom ( Hydrogen )
    , spawnAtom
    ) where

import System.Random
import Particle
import Physics
import Math
import NumberTypes

data Atom = Hydrogen | Helium

-- Spawns atom at position
spawnAtom :: Atom -> Vector3D PreciseNum -> Vector3D PreciseNum -> [Particle] -> [Particle]
spawnAtom Hydrogen pos vel ps = proton : electron : ps
    where
        proton = Particle Proton pos vel
        electron = Particle Electron (pos + Vector3D 0 bohrsRadius 0) vel
