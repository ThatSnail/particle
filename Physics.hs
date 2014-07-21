module Physics (
      coulombsConst
    , bohrsRadius
    , TimeStep
    ) where

import NumberTypes

type TimeStep = Double

-- Coulomb's constant 1 / (4 * pi * e0)
--  where e0 is the permittivity of free space
coulombsConst :: (Floating a) => a
coulombsConst = 8.9875517873681764 * 10 ^ 9

-- Bohr's radius in meters
bohrsRadius :: (Floating a) => a
bohrsRadius = 5.29 * 10 ^ (-11)
