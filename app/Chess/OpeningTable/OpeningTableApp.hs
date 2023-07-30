module Chess.OpeningTable.OpeningTableApp where

import Chess.Evaluation.EvaluationServantApi
import Network.Wai.Handler.Warp

main = do
  app <- runEvalServer "./config/opening-table-config.yaml" "./config/piece-weightings.yaml"
  run 3000 app
