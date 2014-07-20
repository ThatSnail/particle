module Physics (
      coulombsConst
    , bohrsRadius
    , TimeStep
    ) where

type TimeStep = Double

-- Coulomb's constant 1 / (4 * pi * e0)
--  where e0 is the permittivity of free space
coulombsConst :: Double
coulombsConst = 8.9875517873681764 * 10 ^ 9

-- Bohr's radius in meters
bohrsRadius :: Double
bohrsRadius = 5.29 / (10 ^ 11)
