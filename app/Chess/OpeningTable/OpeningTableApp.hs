module Chess.OpeningTable.OpeningTableApp where

import Chess.Evaluation.EvaluationService
import Network.Wai.Handler.Warp

main = do
  app <- createEvalApp "./config/opening-table-config.yaml" Nothing
  run 3000 app
