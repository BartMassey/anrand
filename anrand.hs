-- Copyright © 2016 Bart Massey
-- [This work is made available under the "MIT License".
-- Please see the file LICENSE in this distribution for
-- license details.]

-- Analysis tool for a Hardware Random Number Generator

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
import System.Console.ParseArgs
import System.Directory
import System.FilePath
import System.Random
import Text.Printf

analysisDir :: FilePath
analysisDir = "analysis"

wWindowSize :: Int
wWindowSize = 4096

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
          plot_points_style .~ filledCircles 0.5 (opaque gray) $
          plot_points_values .~ samplePoints $
          plot_points_title .~ "all" $
          def
  let zoomedPlot =
          toPlot $
          plot_lines_style . line_color .~ opaque black $
          plot_lines_values .~ [zoomedPoints] $
          plot_lines_title .~ "zoomed" $
          def
  let tsLayout =
          layout_x_axis .~ (laxis_title .~ "sample" $ def) $
          layout_y_axis .~ (laxis_title .~ "value" $ def) $
          layout_plots .~ [allPlot, zoomedPlot] $
          def
  layoutToRenderable tsLayout

plotSampleHist :: Int -> [Int] -> Renderable (LayoutPick Int Int Int)
plotSampleHist nBins samples = do
  let histPlot =
          plotBars $
          plot_bars_values .~  sampleBars ++ [(nBins, [0])] $
          plot_bars_item_styles .~ [barStyle] $
          plot_bars_spacing .~ BarsFixGap 0 0 $
          plot_bars_alignment .~ BarsLeft $
          def
  let histLayout =
          layout_x_axis .~ (laxis_title .~ "value" $ def) $
          layout_y_axis .~ (laxis_title .~ "frequency" $ def) $
          layout_plots .~ [histPlot] $
          def
  layoutToRenderable histLayout
  where
    sampleBars =
        map (\(x, y) -> (x, [y])) $ rawHist samples
    barStyle = (FillStyleSolid (opaque gray),
                Just (line_width .~ 0.05 $ line_color .~ opaque black $ def))

plotSampleDFT :: [Double] -> Renderable (LayoutPick Double Double Double)
plotSampleDFT dftBins = do
  let dftPlot =
          plotBars $
          plot_bars_spacing .~ BarsFixWidth 0.3 $
          plot_bars_values .~ dftBars $
          plot_bars_item_styles .~ [barStyle] $
          def
  let dftLayout =
          layout_x_axis .~ (laxis_title .~ "frequency" $ def) $
          layout_y_axis .~ (laxis_title .~ "amplitude" $ def) $
          layout_plots .~ [dftPlot] $
          def
  layoutToRenderable dftLayout
  where
    dftBars = map (\(x, y) -> (x, [y])) $ zip [0..] dftBins
    barStyle = (FillStyleSolid (opaque black), Nothing)

data EntropyMode = EntropyModeRaw | EntropyModeNormalized

entropy :: Real a => EntropyMode -> [a] -> Double
entropy mode samples =
    negate $ sum $ map binEntropy samples
    where
      weight =
          case mode of
            EntropyModeRaw -> 1.0
            EntropyModeNormalized -> realToFrac $ sum samples
      binEntropy 0 = 0
      binEntropy count =
          p * logBase 2 p
          where
            p = realToFrac count / weight

hannWindow :: Int -> [Double]
hannWindow nSamples =
    map hannFunction [0 .. nSamples - 1]
    where
      hannFunction n =
          0.5 * (1.0 - cos(2 * pi * fromIntegral n / nn))
          where
            nn = fromIntegral (nSamples - 1)

sampleDFT :: [Double] -> [Double]
sampleDFT samples =
    map magnitude $ V.toList $ run dftR2C $ V.fromList samples

data Bias = BiasDebiased | BiasNominal Int
data DFTMode = DFTModeRaw |
               DFTModeProper Int (Int -> [Double])

processDFT :: Bias -> DFTMode -> [Int] -> [Double]
processDFT bias dftMode samples =
    case dftMode of
      DFTModeRaw ->
          sampleDFT dftSamples
          where
            dftStart = (nSamples - dftLength) `div` 3
            dftLength = min 10000 nSamples
            dftSamples = takeSpan dftStart dftLength normedSamples
      DFTModeProper windowSize window ->
        avgBins $ map (sampleDFT . applyWindow) $
          splitSamples normedSamples
        where
          applyWindow xs =
              zipWith (*) xs $ window windowSize
          splitSamples xs
              | length first < windowSize = []
              | otherwise =
                  first : splitSamples rest
              where
                (first, rest) = splitAt windowSize xs
          avgBins bins =
              map average $ transpose bins
    where
      nSamples = length samples
      normedSamples =
          map norm samples
          where
            norm sample =
                (fromIntegral sample - dftDC) / fromIntegral nSamples
                where
                  dftDC = 
                      case bias of
                        BiasNominal nBits ->
                            fromIntegral (2 ^ (nBits - 1) - 1 :: Integer)
                        BiasDebiased ->
                            average samples

average :: Real a => [a] -> Double
average samples =
    realToFrac (sum samples) / fromIntegral (length samples)

spectralFlatness :: [Double] -> Double
spectralFlatness rDFT =
    10.0 * (gMeanDB - aMeanDB)
    where
      xDFT = tail rDFT
      nxDFT = fromIntegral (length xDFT)
      gMeanDB = sum (map (logBase 10) xDFT) / nxDFT
      aMeanDB = logBase 10 (sum xDFT) - logBase 10 nxDFT

showStats :: Int -> [Int] -> [Double] -> [Double] -> String
showStats nBits samples rDFT wDFT = unlines [
  printf "min: %d" (minimum samples),
  printf "max: %d" (maximum samples),
  printf "mean: %0.3g" (average samples),
  printf "byte-entropy: %0.3g"
      (entropyAdj * entropy EntropyModeNormalized hist),
  printf "spectral-entropy: %0.3g"
      (spectralEntropyAdj * entropy EntropyModeNormalized rDFT2),
  printf "spectral-flatness-db: %0.3g" (spectralFlatness rDFT),
  printf "avg-spectral-flatness-db: %0.3g" (spectralFlatness wDFT) ]
  where
    hist = map snd $ rawHist samples
    rDFT2 = map (**2.0) $ tail rDFT
    entropyAdj =
        max 1.0 $ 8.0 / fromIntegral nBits
    spectralEntropyAdj =
        fromIntegral (max 8 nBits)  / logBase 2.0 (fromIntegral (length rDFT2))

analyze :: Bool -> String -> Int -> [Int] -> IO ()
analyze statsOnly what nBits samples = do
  let rDFT = processDFT BiasDebiased DFTModeRaw samples
  let wDFT = processDFT BiasDebiased
               (DFTModeProper wWindowSize hannWindow) samples
  writeFile (analysisFile what "stats" "txt") $
    showStats nBits samples rDFT wDFT
  when (not statsOnly) $ do
    writeFile (analysisFile what "hist" "txt") $
      showHist $ rawHist samples
    pdfRender (analysisFile what "ts" "pdf") $
      plotTimeSeries samples
    pdfRender (analysisFile what "hist" "pdf") $
      plotSampleHist (2 ^ nBits) samples
    pdfRender (analysisFile what "dft" "pdf") $
      plotSampleDFT rDFT
    pdfRender (analysisFile what "wdft" "pdf") $
      plotSampleDFT wDFT

data ArgIndex = ArgIndexBitsFile
              | ArgIndexStatsOnly
                deriving (Eq, Ord, Show)

argd :: [Arg ArgIndex]
argd = [
  Arg {
    argIndex = ArgIndexStatsOnly,
    argName = Just "stats-only",
    argAbbr = Just 's',
    argData = Nothing,
    argDesc = "Just compute statistics" },
  Arg {
    argIndex = ArgIndexBitsFile,
    argName = Nothing,
    argAbbr = Nothing,
    argData = argDataOptional "bits-file" ArgtypeString,
    argDesc = "Random bits." } ]

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete argd

  let statsOnly = gotArg args ArgIndexStatsOnly

  bitsFile <- getArgStdio args ArgIndexBitsFile ReadMode
  rawSamples <- B.hGetContents bitsFile
  let samples = readSamples rawSamples

  createDirectoryIfMissing True analysisDir

  analyze statsOnly "raw" 12 samples

  prngSamples <- replicateM (length samples) (randomRIO (0, 4095) :: IO Int)
  analyze statsOnly "prng" 12 prngSamples

  let twoBitSamples = map (.&. 0x03) samples
  analyze statsOnly "twobit" 2 twoBitSamples

  let midSamples = map ((.&. 0xff) . (`shiftR` 1)) samples
  analyze statsOnly "mid" 8 midSamples

  let lowSamples = map (.&. 0xff) samples
  analyze statsOnly "low" 8 lowSamples
