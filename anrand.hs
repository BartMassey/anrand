-- Copyright © 2016 Bart Massey
-- [This work is made available under the "MIT License".
-- Please see the file LICENSE in this distribution for
-- license details.]

-- Analysis tool for a Hardware Random Number Generator

import Control.Lens
import Data.Char (toLower)
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Default.Class
import Data.List.Split (splitOn)
import Data.Maybe
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
import System.Exit (exitSuccess)
import System.FilePath
import System.Random
import Text.Printf

defaultAnalysisDir :: FilePath
defaultAnalysisDir = "analysis"

wWindowSize :: Int
wWindowSize = 4096

analysisFile :: FilePath -> FilePath -> String -> String-> FilePath
analysisFile dir what suff ext =
    joinPath [dir, addExtension (what ++ "-" ++ suff) ext]

plotSuffix :: Maybe FileFormat -> String
plotSuffix (Just PDF) = "pdf"
plotSuffix (Just SVG) = "svg"
plotSuffix _ = error "no suffix for (non)plot format"

plotRender :: FileFormat -> String -> Renderable a -> IO ()
plotRender ff name r = do
    _ <- renderableToFile (fo_format .~ ff $ def) name r
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

type ColourPair = (Colour Double, Colour Double)

plotTimeSeries :: ColourPair -> [Int]
               -> Renderable (LayoutPick Int Int Int)
plotTimeSeries (color, color2) samples = do
  let nSamples = length samples
  let samplePoints = zip [(1::Int)..] samples
  let zoomedPoints =
          zip [0, nSamples `div` 100 ..] $
              takeSpan (nSamples `div` 3) 100 samples
  let allPlot =
          toPlot $
          plot_points_style .~ filledCircles 0.5 (opaque color2) $
          plot_points_values .~ samplePoints $
          plot_points_title .~ "all" $
          def
  let zoomedPlot =
          toPlot $
          plot_lines_style . line_color .~ opaque color $
          plot_lines_values .~ [zoomedPoints] $
          plot_lines_title .~ "zoomed" $
          def
  let tsLayout =
          layout_x_axis .~ (laxis_title .~ "sample" $ def) $
          layout_y_axis .~ (laxis_title .~ "value" $ def) $
          layout_plots .~ [allPlot, zoomedPlot] $
          def
  layoutToRenderable tsLayout

plotSampleHist :: ColourPair -> Int -> [Int]
               -> Renderable (LayoutPick Int Int Int)
plotSampleHist (color, _) nBins samples = do
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
                Just (line_width .~ 0.05 $ line_color .~ opaque color $ def))

plotSampleDFT :: ColourPair -> [Double]
              -> Renderable (LayoutPick Double Double Double)
plotSampleDFT (color, _) dftBins = do
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
    barStyle = (FillStyleSolid (opaque color), Nothing)

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

splitSamples :: Int -> [a] -> [[a]]
splitSamples n xs
    | length first < n = []
    | otherwise =
        first : splitSamples n rest
    where
      (first, rest) = splitAt n xs

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
          splitSamples windowSize normedSamples
        where
          applyWindow xs =
              zipWith (*) xs $ window windowSize
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

stdDeviation :: Real a => [a] -> Double
stdDeviation samples =
    sqrt (sum devs / fromIntegral (length devs))
    where
      devs = map (square . (`subtract` mean) . realToFrac) samples
      square x = x * x
      mean = average samples

spectralFlatness :: [Double] -> Double
spectralFlatness rDFT =
    10.0 * (gMeanDB - aMeanDB)
    where
      xDFT = tail rDFT
      nxDFT = fromIntegral (length xDFT)
      gMeanDB = sum (map (logBase 10) xDFT) / nxDFT
      aMeanDB = logBase 10 (sum xDFT) - logBase 10 nxDFT

secondMoment64 :: [Int] -> (Double, Double)
secondMoment64 samples =
    (minimum devs, maximum devs)
    where
      devs = map stdDeviation $ splitSamples 64 samples

showStats :: Int -> [Int] -> [Double] -> [Double] -> String
showStats nBits samples rDFT wDFT = unlines [
  printf "min: %d" (minimum samples),
  printf "max: %d" (maximum samples),
  printf "mean: %0.3g" (average samples),
  printf "byte-entropy: %0.3g"
      (entropyAdj * entropy EntropyModeNormalized hist),
  printf "second-moment-64s-min: %0.3g" smMin,
  printf "second-moment-64s-max: %0.3g" smMax,
  printf "spectral-entropy: %0.3g"
      (spectralEntropyAdj * entropy EntropyModeNormalized rDFT2),
  printf "spectral-flatness-db: %0.3g" (spectralFlatness rDFT),
  printf "avg-spectral-flatness-db: %0.3g" (spectralFlatness wDFT) ]
  where
    (smMin, smMax) = secondMoment64 samples
    hist = map snd $ rawHist samples
    rDFT2 = map (**2.0) $ tail rDFT
    entropyAdj =
        max 1.0 $ 8.0 / fromIntegral nBits
    spectralEntropyAdj =
        fromIntegral (max 8 nBits)  / logBase 2.0 (fromIntegral (length rDFT2))

analyze :: Maybe FileFormat -> ColourPair
        -> String -> String -> Int -> [Int]
        -> IO ()
analyze plotMode colors dir what nBits samples = do
  let rDFT = processDFT BiasDebiased DFTModeRaw samples
  let wDFT = processDFT BiasDebiased (DFTModeProper wWindowSize hannWindow) samples
  writeFile (af "stats" "txt") $
    showStats nBits samples rDFT wDFT
  case plotMode of
    Just ff -> do
      writeFile (af "hist" "txt") $ showHist $ rawHist samples
      let pr = plotRender ff
      pr (af' "ts") $ plotTimeSeries colors samples
      pr (af' "hist") $ plotSampleHist colors (2 ^ nBits) samples
      pr (af' "dft") $ plotSampleDFT colors rDFT
      pr (af' "wdft") $ plotSampleDFT colors wDFT
    Nothing -> return ()
  where
    af = analysisFile dir what
    af' name = af name (plotSuffix plotMode)

data ArgIndex = ArgIndexBitsFile
              | ArgIndexPlotFormat
              | ArgIndexAnalysisDir
              | ArgIndexColor
              | ArgIndexColor2
              | ArgIndexTests
                deriving (Eq, Ord, Show)

maybeReadS :: ReadS a -> String -> Maybe a
maybeReadS f s =
    case f s of
      [(v, "")] -> Just v
      _ -> Nothing

readColor :: String -> Colour Double
readColor name =
    head $ mapMaybe id [
               maybeReadS sRGB24reads name,
               readColourName name,
               error "illegal color" ]

argd :: [Arg ArgIndex]
argd = [
  Arg {
    argIndex = ArgIndexPlotFormat,
    argName = Just "plot-format",
    argAbbr = Just 'p',
    argData = argDataDefaulted "type" ArgtypeString "pdf",
    argDesc = "Plot format (\"pdf\", \"svg\", or \"none\" for just stats)" },
  Arg {
    argIndex = ArgIndexAnalysisDir,
    argName = Just "analysis-dir",
    argAbbr = Just 'a',
    argData = argDataDefaulted "dir" ArgtypeString defaultAnalysisDir,
    argDesc = "Directory for analysis results" },
  Arg {
    argIndex = ArgIndexColor,
    argName = Just "color",
    argAbbr = Nothing,
    argData = argDataDefaulted "name/rgb" ArgtypeString "black",
    argDesc = "Plot color" },
  Arg {
    argIndex = ArgIndexColor2,
    argName = Just "secondary-color",
    argAbbr = Nothing,
    argData = argDataDefaulted "name/rgb" ArgtypeString "gray",
    argDesc = "Plot secondary color" },
  Arg {
    argIndex = ArgIndexTests,
    argName = Just "tests",
    argAbbr = Just 't',
    argData = argDataOptional "test-list" ArgtypeString,
    argDesc = "Comma-separated list of tests (\"help\" for help)" },
  Arg {
    argIndex = ArgIndexBitsFile,
    argName = Nothing,
    argAbbr = Nothing,
    argData = argDataOptional "bits-file" ArgtypeString,
    argDesc = "Random bits." } ]


testSet :: [String]
testSet = ["raw","prng","twobit","mid","mid7","low","hm"]

testHelp :: IO ()
testHelp = do
  printf "Available Tests:\n"
  mapM_ (\t -> printf "  %s\n" t) testSet
  exitSuccess

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete argd

  let tests =
          case getArg args ArgIndexTests of
            Nothing -> testSet
            Just(ts) -> splitOn "," ts
  when (tests == ["help"]) testHelp

  let plotFormat =
          case map toLower $ getRequiredArg args ArgIndexPlotFormat of
            "none" -> Nothing
            "pdf" -> Just PDF
            "svg" -> Just SVG
            _ -> error "unrecognized plot format"

  bitsFile <- getArgStdio args ArgIndexBitsFile ReadMode
  rawSamples <- B.hGetContents bitsFile
  let samples = readSamples rawSamples

  let analysisDir = getRequiredArg args ArgIndexAnalysisDir
  createDirectoryIfMissing True analysisDir

  let plotColor = readColor $ getRequiredArg args ArgIndexColor
  let plotColor2 = readColor $ getRequiredArg args ArgIndexColor2

  let aa name bits source =
          when (name `elem` tests) $
               analyze plotFormat (plotColor, plotColor2) analysisDir
                       name bits source

  aa "raw" 12 samples

  prngSamples <- replicateM (length samples) (randomRIO (0, 4095) :: IO Int)
  aa "prng" 12 prngSamples

  let twoBitSamples = map (.&. 0x03) samples
  aa "twobit" 2 twoBitSamples

  let midSamples = map ((.&. 0xff) . (`shiftR` 1)) samples
  aa "mid" 8 midSamples

  let mid7Samples = map ((.&. 0x7f) . (`shiftR` 2)) samples
  aa "mid7" 7 mid7Samples

  let lowSamples = map (.&. 0xff) samples
  aa "low" 8 lowSamples

  let hmSamples = map hmBits samples
                  where
                    hmBits sample =
                        let mid = (sample `shiftR` 1) .&. 0xff in
                        mid `xor` (sample .&. 0x01) `xor`
                          ((sample `shiftR` 9) .&. 0x01) `xor`
                          ((sample `shiftR` 10) .&. 0x01) `xor`
                          ((sample `shiftR` 11) .&. 0x01)
  aa "hm" 8 hmSamples
