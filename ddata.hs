import Control.Lens
import Data.Maybe
import Data.List
import Data.List.Split
import Data.String.Utils
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Layout
import Graphics.Rendering.Chart.Backend.Cairo
--import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy
import Text.Read

import qualified Data.Map as Map
import qualified Data.Matrix as M
import qualified Data.Text as T
import qualified Data.Vector.Storable as V
import qualified Numeric.Statistics as S

import System.Environment(getArgs)
import Text.CSV
import Text.Parsec.Error

main = do
   csvData <- parseCSVFromFile "data/test-data.csv"
   case csvData of
      Left err
         -> sequence_
            . map (putStrLn . messageString)
            . errorMessages
            $ err
      Right (header:contents)
         -> process header contents
         >> (putStrLn ".")

opt = def{_fo_size=(800, 9000)}

process header contents =
   (mapM_ renderCharts (chunksOf 10 comb))
   >> (mapM_ processCorcoeff comb)
   where
      comb = sortBy sortPairs [ x
         | x <- mapM (const (zip header cols)) [1..2]
         , head x < head (tail x) ]
      cols = M.toLists . M.transpose . M.fromLists
         . (take (length contents - 1)) . trimCSV $ contents

renderCharts comb = renderableToFile opt ("output/" ++ title ++ ".png")
   $ renderStackedLayouts
   $ slayouts_layouts .~ map (StackedLayout . processPair) comb
   $ slayouts_compress_legend .~ False
   $ def
   where
      title = fixFileName $ t1 ++ " --> " ++ t2
         where
            t1 = fst . head . head $ comb
            t2 = fst . head . reverse . head $ comb

sortPairs :: [(String, [String])] -> [(String, [String])] -> Ordering
sortPairs a b = compare left right
   where
      left = fst . head $ a
      right = fst . head $ b

processCorcoeff ((hx, x):(hy, y):_) = putStrLn $
   hx ++ "," ++ hy ++ "," ++ (show cvSpear) ++ "," ++ (show cv)
      where
         cvSpear = S.spearman (V.fromList dx) (V.fromList dy)
         cv = S.corcoeff (V.fromList dx) (V.fromList dy)
         dx = digitize x
         dy = digitize y

processPair ((hx, x):(hy, y):_) = chartPair title pts
   where
      title = hx ++ "-x-" ++ hy
      pts = zip (digitize x) (digitize y)

chartPair title pts = layout_title .~ title
   $ layout_plots .~ [toPlot (
      plot_points_values .~ pts
      $ plot_points_style .~ filledCircles 2 (opaque red)
      $ def
   )]
   $ def

trimCSV = map . map $ trimCol
trimCol = T.unpack . T.strip . T.pack
fixFileName x = replace "." "_" $ replace "/" "_" x

-- data stream to digital data stream
digitize vs = catMaybes $ dz mv vs
   where
      mv = foldl mkValueMap Map.empty vs
      dz mv vs = map g vs where g x = Map.lookup x mv

mkValueMap valMap v = case Map.lookup v valMap of
   Just value -> valMap
   Nothing -> case readMaybe v :: Maybe Double of
      Just fv -> Map.insert v fv valMap
      Nothing -> Map.insert v (fromIntegral (Map.size valMap)) valMap
