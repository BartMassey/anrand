import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Layout
import Graphics.Rendering.Chart.Plot.Histogram
import Graphics.Rendering.Chart.Plot.Lines
import Graphics.Rendering.Chart.Plot.Points

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import Text.Printf

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
            
textHist :: [Int] -> String
textHist samples =
    unlines $ map histify $
     group $ sort $ samples ++ [smallest..largest]
    where
      smallest = minimum samples
      largest = maximum samples
      histify [] = error "bad hist group"
      histify (x : xs) = unwords [show x, show (length xs)]

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

showStats :: [Int] -> String
showStats samples =
  let nSamples = length samples in
  printf "min: %d  max: %d  mean: %g\n"
      (minimum samples)
      (maximum samples)
      (fromIntegral (sum samples) / fromIntegral nSamples :: Double)

plotSamples :: String -> [Int] -> IO ()
plotSamples what samples = do
  writeFile (what ++ "-stats.txt") $ showStats samples
  pdfRender (what ++ "-ts.pdf") $ plotTimeSeries samples
  pdfRender (what ++ "-hist.pdf") $ plotSampleHist samples
  writeFile (what ++ "-hist.txt") $ textHist samples

main :: IO ()
main = do
  rawSamples <- B.getContents
  let samples = readSamples rawSamples

  plotSamples "raw" samples

  let lowSamples = map (.&. 0xff) samples
  plotSamples "low" lowSamples

  let midSamples = map ((.&. 0xff) . (`shiftR` 1)) samples
  plotSamples "mid" midSamples
