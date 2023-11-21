{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chess.Evaluation.ServantTypeclassInstances where

import Chess.Fen
import Chess.Fen.FenParser
import Control.Arrow (left)
import Control.Monad (join)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import GHC.Generics
import Servant.API
import qualified Servant.API.Stream as ServantStream
import qualified Servant.Types.SourceT as ServantStream
import qualified Streamly.Internal.Data.Stream.StreamK as Stream

instance MimeRender PlainText FenRepresentation where
  mimeRender _ = pack . show

instance MimeUnrender PlainText FenRepresentation where
  mimeUnrender _ = left show . parseFen . unpack

instance ServantStream.FromSourceIO a (Stream.Stream IO a) where
  fromSourceIO src = join $ Stream.mapM id $ Stream.fromPure $ ServantStream.unSourceT src go
    where
      go :: ServantStream.StepT IO a -> IO (Stream.Stream IO a)
      go step =
        case step of
          ServantStream.Stop -> return Stream.nil
          ServantStream.Error e -> return $ Stream.fromEffect $ error e
          ServantStream.Skip n -> go n
          ServantStream.Yield x nextStep -> Stream.cons x <$> go nextStep
          ServantStream.Effect nextStep -> nextStep >>= go
  -- {-# SPECIALIZE INLINE fromSourceIO :: Stream.IsStream t => ServantStream.SourceIO a -> t IO a #-}

instance ServantStream.ToSourceIO a (Stream.Stream IO a) where
  toSourceIO stream = ServantStream.SourceT ($ transform stream)
    where
      transform = ServantStream.Effect . Stream.foldr ServantStream.Yield ServantStream.Stop
