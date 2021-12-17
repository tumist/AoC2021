-- input: target area: x=192..251, y=-89..-59
-- When the rocket is launched in the positive y-direction,
-- it will appear again at y = 0 and the next step is equal to
-- one more of the initial y direction, but negative.
-- This means that to get within -89, the inital velocity upwards
-- can at most be 88. The top height of the rocket is then:
main :: IO ()
main = print $ sum [0..88]