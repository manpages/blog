{-# LANGUAGE OverloadedStrings #-}

module Blog () where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map             (Map (..))
import qualified Data.Map             as M
import           Data.Maybe           (fromJust)
import           Data.Text            (pack, unpack)
import qualified Data.Text            as T
import           Turtle

-- | Currently we use stringly-typed values everywhere
-- TODO: smart constructors, better types
type Cmd         = Text
-- | Currently we use stringly-typed values everywhere
-- TODO: smart constructors, better types
type File        = Text
-- | Currently we use stringly-typed values everywhere
-- TODO: smart constructors, better types
type Remote      = Text
-- | Currently we use stringly-typed values everywhere
-- TODO: smart constructors, better types
type Path        = Text

-- |
-- Blog connectivity is configured with this type.
-- Keys are other types from this file as Text,
-- values are Text.
type Blog = ReaderT (Map Text Text) IO Bool

-- | Rsync a file using Turtle's ``shell`` function.
rsyncSend :: Cmd -> File -> Remote -> Path -> IO ExitCode
rsyncSend ssh file remote path =
  she $ "rsync -Pave" <^> q ssh <^> sq file <^> spc remote <^> cq path

-- | Synonim for ``Text.append``
(<^>) :: Text -> Text -> Text
(<^>) = T.append

-- | Quote a string (place it into double quotes)
q :: Text -> Text
q = T.append "\"" . flip T.append "\""

-- | Prefix a string with a space
spc :: Text -> Text
spc = T.append " "

-- | Prefix a string with a space and quote it.
sq :: Text -> Text
sq = spc . q

-- | Prefix a string with a colon.
c :: Text -> Text
c = T.append ":"

-- | Prefix a string with a colon and quote it.
cq :: Text -> Text
cq = c . q

-- | Shorthand for executing a command under empty shell.
she :: Text -> IO ExitCode
she = (flip shell) empty

-- | Get rsync configuration out of our map
rsyncTup :: Map Text Text -> (Cmd, File, Remote, Path)
rsyncTup x = (f "Cmd" x, f "File" x, f "Remote" x, f "Path" x)
  where
    f k vs = fromJust $ M.lookup k vs
