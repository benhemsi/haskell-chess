module Chess.OpeningTable.OpeningTableApp where

import Chess.Evaluation.EvaluationApi
import Chess.Evaluation.EvaluationConfig (PieceWeightings(PieceWeightings))
import Control.Concurrent
import qualified Data.Yaml as Y
import Network.Wai.Handler.Warp

main = do
  app <- runEvalServer "./config/opening-table-config.yaml" "./config/piece-weightings.yaml"
  run 3000 app
