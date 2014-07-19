import Graphics.Gloss

w :: Int
w = 500

h :: Int
h = 500

main = display (InWindow "Particle Simulator" (w, h) (10, 10)) white (Circle 80)
