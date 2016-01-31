import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Complex
import qualified Data.Vector.Unboxed as V
import Numeric.FFT.Vector.Unnormalized

import Control.Monad
import Data.Bits
import qualified Data.ByteString as B
import Data.List
import System.Directory
import System.FilePath
import System.Random
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
            
takeSpan :: Int -> Int -> [a] -> [a]
takeSpan start len xs =
    take len $ drop start xs

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
              takeSpan (nSamples `div` 3) 100 samples
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

plotSampleHist :: Int -> [Int] -> Renderable (LayoutPick Int Int Int)
plotSampleHist nBins samples = do
  let histPlot =
          plotBars $
          plot_bars_titles .~ ["Sample Histogram"] $
          plot_bars_values .~  sampleBars ++ [(nBins, [0])] $
          plot_bars_item_styles .~ [(FillStyleSolid (opaque red), Nothing)] $
          plot_bars_spacing .~ BarsFixGap 0 0 $
          plot_bars_alignment .~ BarsLeft $
          def
  let histLayout =
          layout_plots .~ [histPlot] $
          def
  layoutToRenderable histLayout
  where
    sampleBars =
        map (\(x, y) -> (x, [y])) $ rawHist samples

plotSampleDFT :: [Int] -> Renderable (LayoutPick Double Double Double)
plotSampleDFT samples = do
  let dftPlot =
          plotBars $
          plot_bars_spacing .~ BarsFixWidth 0.1 $
          plot_bars_values .~ sampleBars $
          plot_bars_titles .~ ["Relative Frequencies"] $
          plot_bars_item_styles .~ [(FillStyleSolid (opaque red), Nothing)] $
          def
  let dftLayout =
          layout_title .~ "DFT (magnitude)" $
          layout_plots .~ [dftPlot] $
          def
  layoutToRenderable dftLayout
  where
    nSamples = length samples
    dftLength = min 10000 nSamples
    dftStart = (nSamples - dftLength) `div` 3
    dftSamples = takeSpan dftStart dftLength samples
    normalizedSamples =
        map norm dftSamples
        where
          dftDC = average dftSamples
          norm sample = fromIntegral sample - dftDC
    sampleBars =
        map (\(x, y) -> (x, [y])) $ zip [0..] $ sampleDFT normalizedSamples

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

sampleDFT :: [Double] -> [Double]
sampleDFT samples =
    map magnitude $ V.toList $ run dftR2C $ V.fromList samples

average :: Integral a => [a] -> Double
average samples =
    fromIntegral (sum samples) / fromIntegral (length samples)

showStats :: Int -> [Int] -> String
showStats nBits samples =
  printf "min: %d  max: %d  mean: %0.3g  byte-entropy: %0.3g\n"
      (minimum samples)
      (maximum samples)
      (average samples)
      (entropyAdj * entropy (rawHist samples))
  where
    nSamples = length samples
    entropyAdj = max 1.0 $ 8.0 / fromIntegral nBits

analyze :: String -> Int -> [Int] -> IO ()
analyze what nBits samples = do
  writeFile (analysisFile what "stats" "txt") $
    showStats nBits samples
  writeFile (analysisFile what "hist" "txt") $
    showHist $ rawHist samples
  pdfRender (analysisFile what "ts" "pdf") $
    plotTimeSeries samples
  pdfRender (analysisFile what "hist" "pdf") $
    plotSampleHist (2 ^ nBits) samples
  pdfRender (analysisFile what "dft" "pdf") $
    plotSampleDFT samples

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

  prngSamples <- replicateM (length samples) (randomRIO (0, 2047) :: IO Int)
  analyze "prng" 12 prngSamples
