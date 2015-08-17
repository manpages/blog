{-# LANGUAGE OverloadedStrings #-}

module Blog (post, stamp, defaultPathFn, knownCategory, rsyncTup, rsyncSend) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map             (Map (..))
import qualified Data.Map             as M
import           Data.Maybe           (fromJust)
import           Data.Text            (pack, unpack)
import qualified Data.Text            as T
import           Data.Time.Clock
import           Data.Time.Format
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
-- | Currently we use stringly-typed values everywhere
-- TODO: smart constructors, better types
type BlogCat     = Text
-- | Text -> Text configuration part
type TextText    = Map Text Text
-- | Text -> [Text] configuration part
type TextTexts   = Map Text [Text]
-- | Configuration to be asked from Reader
data BlogCfg = BlogCfg { connection :: TextText                        -- ^ Describes how to connect to the blog, for now specifies rsync args
                       , blog_spec  :: TextTexts                       -- ^ Describes the categories (and, later, tags) allowed in the blog
                       , pathFn     :: BlogCat -> Path -> Path -> Path -- ^ Builds remote path based on category, local path and remote base path
                       }

-- |
-- Blog connectivity is configured with this type.
-- Keys are other types from this file as Text,
-- values are Text.
type Blog = ReaderT BlogCfg IO ExitCode

-- |
-- With BlogCfg run ReaderT
post :: BlogCfg -> Path -> BlogCat -> Blog
post x y z = runReaderT (postR y z) x

-- |
-- Based on our ReaderT, posts an entry to the blog, if category is ok
postR :: Path -> BlogCat -> Blog
postR x c = do
  (BlogCfg { connection = con
           , blog_spec  = spec
           , pathFn     = f}) <- ask
  let (cmd, rem, pth) = rsyncTup con
  tau <- lift $ getCurrentTime
  if knownCategory c spec
    then lift $ rsyncSend cmd x rem $ f c x pth
    else return $ ExitFailure 5

-- | Default path function (works with my blog :))
defaultPathFn :: FormatTime t => BlogCat -> Path -> Path -> t -> Path
defaultPathFn "draft" l r t = r <^> slash "drafts"             <^> (slash $ stamp l t)
defaultPathFn c       l r t = r <^> slash "posts"  <^> slash c <^> (slash $ stamp l t)

-- | Prefix a filename with a default hakyll timestamp
stamp :: FormatTime t => Text -> t -> Text
stamp x tau = x <^> (pack $ formatTime defaultTimeLocale "%Y-%m-%d" tau)

-- | Rsync a file using Turtle's ``shell`` function.
rsyncSend :: Cmd -> File -> Remote -> Path -> IO ExitCode
rsyncSend ssh file remote path =
  she $ "rsync -Pave" <^> q ssh <^> sq file <^> spc remote <^> cq path <^> ".markdown"

-- | Looks into blog specification and tells if argument is a known category
knownCategory x spec = x `elem` xs
  where
    xs = fromJust $ M.lookup "BlogCat" spec

-- | Synonim for ``Text.append``
(<^>) :: Text -> Text -> Text
(<^>) = T.append

-- | Quote a string (place it into double  quotes)
q :: Text -> Text
q = T.append "\"" . flip T.append "\""

-- | Prefix a string with a slash
slash :: Text -> Text
slash = T.append "/"

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
rsyncTup :: Map Text Text -> (Cmd, Remote, Path)
rsyncTup x = (f "Cmd", f "Remote", f "Path")
  where
    f k = fromJust $ M.lookup k x
