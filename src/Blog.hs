{-# LANGUAGE OverloadedStrings #-}

module Blog () where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map               (Map (..))
import qualified Data.Map               as M
import           Data.Text              (pack, unpack)
import qualified Data.Text              as T
import           Turtle

type Cmd         = Text
type File        = Text
type Remote      = Text
type Path        = Text

type Blog = ReaderT (Map Text Text) IO Bool

rsyncSend :: Cmd -> File -> Remote -> Path -> IO ExitCode
rsyncSend ssh file remote path = shell ssh empty
--  shell "rsync -Pave" <> q ssh <> sq file <> spc remote <> cq path

q :: Text -> Text
q = T.append "\"" . flip T.append "\""

spc :: Text -> Text
spc = T.append " "

sq :: Text -> Text
sq = spcs . q

c :: Text -> Text
c = T.append ":"

cq :: Text -> Text
cq = c . q
