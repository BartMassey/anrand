import qualified Data.Vector as V
import Graphics.Rendering.Plot
import Numeric.FFT.Vector.Unnormalized
import Data.Bits
import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

readSamples :: B.ByteString -> [Int]
readSamples stuff =
    makeInts $ B.unpack stuff
    where
      makeInts [] = []
      makeInts [_] = error "odd byte array"
      makeInts (b2 : b1 : bs) =
          let i1 = fromIntegral b1
              i2 = fromIntegral b2
          in
          ((i1 `shiftL` 8) .|. i2) : makeInts bs
            
textHist :: B.ByteString -> B.ByteString
textHist stuff =
    BC.pack $ unlines $ map printbin $ group $
    sort $ (++ [0..(4095 :: Int)]) $ readSamples stuff
    where
      printbin [] = error "missing value"
      printbin (x : xs) =
          unwords [show x, show $ length xs]

main :: IO ()
main = B.interact textHist
