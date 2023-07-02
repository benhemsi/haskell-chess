module Chess.ChessApp where

import Chess.Fen
import Chess.OpeningTable.OpeningTableApi
import Control.Concurrent (threadDelay)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client

query :: ClientM (Maybe Double)
query = getOpeningPositionEvaluation startingFenRepresentation

main :: IO ()
main = do
  threadDelay 2000000
  manager <- newManager defaultManagerSettings
  res <- runClientM query (mkClientEnv manager (BaseUrl Http "opening-table" 3000 ""))
  print res
