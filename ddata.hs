--import Graphics.Rendering.Chart
--import Graphics.Rendering.Chart.Backend.Cairo
--import Data.Default.Class
--import Control.Lens
--import System.Environment(getArgs)
import Text.CSV
import Text.Parsec.Error
import qualified Data.Text as T
import qualified Data.Matrix as M
import qualified Data.Map as Map
import Data.Maybe

--chart = toRenderable layout
--   where
--      values = [ ("Mexico City", 19.2, e),
--         ("Mumbai", 12.9, e),
--         ("Sydney", 4.3, e),
--         ("London", 8.3, e),
--         ("New York", 8.2, e1),
--         ("Beijing", 15.2, e) ]
--      e = 0
--      e1 = 25
--      pitem (s,v,o) = pitem_value .~ v
--         $ pitem_label .~ s
--         $ pitem_offset .~ o
--         $ def
--      layout = pie_title .~ "Relative Population"
--         $ pie_plot . pie_data .~ map pitem values
--         $ def

--main1 ["svg"] = renderableToFile def "example5.svg" chart
--main1 ["png"] = renderableToFile def "example5.png" chart

--main = getArgs >>= main1

main :: IO ()
main = do
   csvData <- parseCSVFromFile "data/data.csv"
   case csvData of
      Left err
         -> sequence_
            . map (putStrLn . messageString)
            . errorMessages
            $ err
      Right (header:contents)
          -> mapM_ processPair comb
         -- -> putStrLn . show $ comb
            where
               comb = [ x
                  | x <- mapM (const (zip header cols)) [1..2]
                  , head x < head (tail x) ]
               cols = M.toLists . M.transpose . M.fromLists
                  . (take (length contents - 1)) . trimCSV $ contents

processPair ((hx, x):(hy, y):_) = do
   (putStrLn $ hx ++ " vs. " ++ hy)
       >> (putStrLn . show . df $ x)
       >> (putStrLn . show . df $ y)
      where
         df vs = catMaybes $ digitize mv vs
            where mv = foldl mkValueMap Map.empty vs

trimCSV = map . map $ trimCol
trimCol = T.unpack . T.strip . T.pack

-- data stream to digital data stream
digitize mv vs = map g vs where g x = Map.lookup x mv

mkValueMap valMap v = case Map.lookup v valMap of
   Just value -> valMap
   Nothing -> Map.insert v (Map.size valMap) valMap
