import Data.Complex 

splitEvenOdd :: [a] -> ([a], [a])
splitEvenOdd = helper ([], [])
  where
    helper acc []                = acc
    helper (acc1, acc2) (x:y:xs) = helper (acc1 ++ [x], acc2 ++ [y]) xs
    helper (acc1, acc2) [x]      = (acc1 ++ [x], acc2)

dft :: [Double] -> Matrix (Complex Double)
dft xs = M.multStd m x
  where
    l = length xs
    x :: Matrix (Complex Double)
    x = M.fromList l 1 (map (\x -> x :+ 0.0) xs)
    m :: Matrix (Complex Double)
    m = M.matrix l l $ \(i, j) -> exp((0.0 :+ (-2.0)) * pi * (fromIntegral i) * (fromIntegral j) / (fromIntegral l))

fft :: [Double] -> Matrix (Complex Double)
fft x | length x <= 32 = dft x
fft x = error "TODO"
  where
    l      = length x
    s      = splitEvenOdd x
    xEven  = fft $ fst s
    xOdd   = fft $ snd s
    factor = map (\i -> (0.0 :+ (-2.0)) * pi * i / l) [1..l]

main :: IO ()
main = do
  s <- getLine
  let
    x = read s :: [Double]
  print $ dft x
