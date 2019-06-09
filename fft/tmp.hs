import Prelude hiding (length, sum, zipWith)
import Data.Complex
import Data.Vector hiding (filter)

omega :: Int -> Complex Double
omega n = cis (2 * pi / fromIntegral n)

dft :: Vector (Complex Double) -> Vector (Complex Double)
dft h = generate n dftHelper
    where
        n = length h
        w = omega n
        dftHelper x = sum . zipWith (*) h . generate n $ (\k -> w ^^ (x * k))

main :: IO ()
main = do
  s <- getLine
  let
    x = read . filter (/= ' ') $ s :: [Double]
    xs = fromList . fmap (:+ 0) $ x
  print $ dft xs
