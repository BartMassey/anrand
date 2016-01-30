import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import Text.Printf

analysis :: String -> String -> String
analysis what suff = "analysis/" ++ what ++ suff

pdfRender :: String -> Renderable a -> IO ()
pdfRender name r = do
    renderableToFile (fo_format .~ PDF $ def) name r
    return ()

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
            
rawHist :: [Int] -> [(Int, Int)]
rawHist samples =
    map histify $ group $ sort $ samples ++ [smallest..largest]
    where
      smallest = minimum samples
      largest = maximum samples
      histify [] = error "bad hist group"
      histify (x : xs) = (x, length xs)

showHist :: [(Int, Int)] -> String
showHist bins =
    unlines $ map showBin bins
    where
      showBin (x, y) = unwords [show x, show y]

plotTimeSeries :: [Int] -> Renderable (LayoutPick Int Int Int)
plotTimeSeries samples = do
  let nSamples = length samples
  let samplePoints = zip [(1::Int)..] samples
  let zoomedPoints =
          zip [0, nSamples `div` 100 ..] $
              take 100 $ drop (nSamples `div` 3) samples
  let allPlot =
          toPlot $
          plot_points_style .~ filledCircles 0.5 (opaque green) $
          plot_points_values .~ samplePoints $
          plot_points_title .~ "all" $
          def
  let zoomedPlot =
          toPlot $
          plot_lines_style . line_color .~ opaque red $
          plot_lines_values .~ [zoomedPoints] $
          plot_lines_title .~ "zoomed" $
          def
  let tsLayout =
          layout_title .~ "Time Series" $
          layout_plots .~ [allPlot, zoomedPlot] $
          def
  layoutToRenderable tsLayout

plotSampleHist :: [Int] -> Renderable (LayoutPick Double Int Int)
plotSampleHist samples = do
  let histPlot =
          histToPlot $
          plot_hist_title .~ "Sample Histogram" $
          plot_hist_bins .~ maximum samples - minimum samples + 1 $
          plot_hist_values .~  map fromIntegral samples $
          (defaultPlotHist :: PlotHist Double Int)
  let histLayout =
          layout_plots .~ [histPlot] $
          def
  layoutToRenderable histLayout

entropy :: [(Int, Int)] -> Double
entropy samples =
    negate $ sum $ map binEntropy samples
    where
      nStates = sum $ map snd samples
      binEntropy (_, 0) = 0
      binEntropy (_, count) =
          p * logBase 2 p
          where
            p = fromIntegral count / fromIntegral nStates

-- The specific data we're looking at has a mod 4 entropy problem.
-- We'll deal with it because we need to.
mod4entropy :: [Int] -> Double
mod4entropy samples =
    entropy mod4sums
    where
      mod4sums = sumMod 4 $ rawHist samples
      sumMod :: Int -> [(Int, Int)] -> [(Int, Int)]
      sumMod m [] = zip [0..m - 1] $ replicate m 0
      sumMod m ((x, y) : xs) =
          map (addBin (x `mod` m, y)) $ sumMod m xs
          where
            addBin :: (Int, Int) -> (Int, Int) -> (Int, Int)
            addBin (i, j) (i', j') | i == i' = (i, j + j')
            addBin (_, _) (i', j') = (i', j')
                

showStats :: [Int] -> String
showStats samples =
  let nSamples = length samples in
  printf "min: %d  max: %d  mean: %0.3g\nentropy: %0.3g  mod4entropy: %0.3g\n"
      (minimum samples)
      (maximum samples)
      (fromIntegral (sum samples) / fromIntegral nSamples :: Double)
      (entropy $ rawHist samples)
      (mod4entropy samples)

analyze :: String -> [Int] -> IO ()
analyze what samples = do
  writeFile (analysis what "-stats.txt") $ showStats samples
  pdfRender (analysis what "-ts.pdf") $ plotTimeSeries samples
  pdfRender (analysis what "-hist.pdf") $ plotSampleHist samples
  writeFile (analysis what "-hist.txt") $ showHist $ rawHist samples

main :: IO ()
main = do
  rawSamples <- B.getContents
  let samples = readSamples rawSamples

  analyze "raw" samples

  let lowSamples = map (.&. 0xff) samples
  analyze "low" lowSamples

  let midSamples = map ((.&. 0xff) . (`shiftR` 1)) samples
  analyze "mid" midSamples
