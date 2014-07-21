module Math (
      Vector3D ( Vector3D )
    , dist
    , mag
    , norm
    ) where

data Vector3D a = Vector3D a a a deriving (Show, Eq)

instance (Floating a) => Num (Vector3D a) where
    Vector3D x1 y1 z1 + Vector3D x2 y2 z2 = Vector3D (x1 + x2) (y1 + y2) (z1 + z2)
    Vector3D x1 y1 z1 - Vector3D x2 y2 z2 = Vector3D (x1 - x2) (y1 - y2) (z1 - z2)
    Vector3D x1 y1 z1 * Vector3D x2 y2 z2 = Vector3D (x1 * x2) (y1 * y2) (z1 * z2)
    abs (Vector3D x y z) = Vector3D (abs x) (abs y) (abs z)
    signum (Vector3D x y z) = Vector3D (signum x) (signum y) (signum z)
    fromInteger a = Vector3D (fromInteger a) (fromInteger a) (fromInteger a)

instance (Floating a) => Fractional (Vector3D a) where
    Vector3D x1 y1 z1 / Vector3D x2 y2 z2 = Vector3D (x1 / x2) (y1 / y2) (z1 / z2)
    fromRational a = Vector3D (fromRational a) (fromRational a) (fromRational a)

-- Distance between two points in 3D space
dist :: (Floating a) => Vector3D a -> Vector3D a -> a
dist (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2)

-- Magnitude of vector
mag :: (Floating a) => Vector3D a -> a
mag v = dist v (fromIntegral 0)

-- Normalize vector so abs is 1
norm :: (Floating a) => Vector3D a -> Vector3D a
norm v = v / (Vector3D m m m)
    where
        m = mag v
