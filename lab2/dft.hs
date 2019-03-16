import Prelude hiding (length, sum, zipWith)
import Data.Complex
import Data.Vector

omega :: Int -> Complex Double
omega n = cis (2 * pi / fromIntegral n)

dft :: Vector (Complex Double) -> Vector (Complex Double)
dft h = generate n helper
    where
        n = length h
        w = omega n
        helper x = sum $ zipWith (*) h $ generate n (\k -> w ^^ (x * k))

main :: IO ()
main = do
  s <- getLine
  let
    x = read . Prelude.filter (/= ' ') $ s :: [Double]
    xs = fromList . fmap (:+ 0) $ x
  print $ dft xs
