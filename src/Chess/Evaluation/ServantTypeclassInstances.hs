{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Chess.Evaluation.ServantTypeclassInstances where

import Chess.Fen
import Chess.Fen.FenParser
import Control.Arrow (left)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import GHC.Generics
import Servant.API
import qualified Servant.API.Stream as ServantStream
import qualified Servant.Types.SourceT as ServantStream
import qualified Streamly.Prelude as Stream

data MinAndMaxEval =
  MinAndMaxEval
    { minFen, maxFen :: (Double, FenRepresentation)
    }
  deriving (Show, Generic)

instance ToJSON MinAndMaxEval

instance FromJSON MinAndMaxEval

instance MimeRender PlainText FenRepresentation where
  mimeRender _ = pack . show

instance MimeUnrender PlainText FenRepresentation where
  mimeUnrender _ = left show . parseFen . unpack

class StreamlyToSourceIO m where
  streamlyToSourceIO :: Stream.IsStream t => t m a -> SourceIO a

instance StreamlyToSourceIO IO where
  streamlyToSourceIO stream = ServantStream.SourceT ($ transform $ Stream.adapt stream)
    where
      transform = ServantStream.Effect . Stream.foldr ServantStream.Yield ServantStream.Stop

instance (StreamlyToSourceIO m, Stream.IsStream t) => ServantStream.ToSourceIO a (t m a) where
  toSourceIO = streamlyToSourceIO

instance (Stream.IsStream t) => ServantStream.FromSourceIO a (t IO a) where
  fromSourceIO src = Stream.concatMapM id $ Stream.fromPure $ ServantStream.unSourceT src go
    where
      go :: Stream.IsStream t => ServantStream.StepT IO a -> IO (t IO a)
      go step =
        case step of
          ServantStream.Stop -> return Stream.nil
          ServantStream.Error e -> return $ Stream.fromEffect $ error e
          ServantStream.Skip n -> go n
          ServantStream.Yield x nextStep -> Stream.cons x <$> go nextStep
          ServantStream.Effect nextStep -> nextStep >>= go
  -- {-# SPECIALIZE INLINE fromSourceIO :: Stream.IsStream t => ServantStream.SourceIO a -> t IO a #-}
