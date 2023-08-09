module Chess.OpeningTable.OpeningTableApp where

import Chess.Evaluation.EvaluationService
import Network.Wai.Handler.Warp

main = do
  app <- createEvalApp "./config/opening-table-config.yaml" "./config/piece-weightings.yaml"
  run 3000 app
