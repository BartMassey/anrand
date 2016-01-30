import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Bits
import qualified Data.ByteString as B
import Data.List
import System.Directory
import System.FilePath
import Text.Printf

analysisDir :: FilePath
analysisDir = "analysis"

analysisFile :: FilePath -> String -> String-> FilePath
analysisFile what suff ext =
    joinPath [analysisDir, addExtension (what ++ "-" ++ suff) ext]

pdfRender :: String -> Renderable a -> IO ()
pdfRender name r = do
    _ <- renderableToFile (fo_format .~ PDF $ def) name r
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

plotSampleHist :: Int -> [Int] -> Renderable (LayoutPick Double Int Int)
plotSampleHist nBins samples = do
  let histPlot =
          histToPlot $
          plot_hist_title .~ "Sample Histogram" $
          plot_hist_bins .~ nBins $
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

showStats :: Int -> [Int] -> String
showStats nBits samples =
  printf "min: %d  max: %d  mean: %0.3g  byte-entropy: %0.3g\n"
      (minimum samples)
      (maximum samples)
      (fromIntegral (sum samples) / fromIntegral nSamples :: Double)
      (entropyAdj * entropy (rawHist samples))
  where
    nSamples = length samples
    entropyAdj = max 1.0 $ 8.0 / fromIntegral nBits

analyze :: String -> Int -> [Int] -> IO ()
analyze what nBits samples = do
  writeFile (analysisFile what "stats" "txt") $
    showStats nBits samples
  pdfRender (analysisFile what "ts" "pdf") $
    plotTimeSeries samples
  pdfRender (analysisFile what "hist" "pdf") $
    plotSampleHist (2 ^ nBits) samples
  writeFile (analysisFile what "hist" "txt") $
    showHist $ rawHist samples

main :: IO ()
main = do
  createDirectoryIfMissing True analysisDir

  rawSamples <- B.getContents
  let samples = readSamples rawSamples

  analyze "raw" 12 samples

  let lowSamples = map (.&. 0xff) samples
  analyze "low" 8 lowSamples

  let midSamples = map ((.&. 0xff) . (`shiftR` 1)) samples
  analyze "mid" 8 midSamples

  let twoBitSamples = map (.&. 0x03) samples
  analyze "twobit" 2 twoBitSamples
