import Data.Matrix



main = return (matrix 4 4 $ \(i, j) -> i == j)