{-# LANGUAGE OverloadedStrings #-}

module Blog (withBlog, post) where

import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Text              (pack, unpack)
import           Turtle

-- This code is stringly typed!

-- | Ditto
type BlogCat     = String
-- | Ditto
type BlogTag     = String
-- | Ditto
type URL         = String
-- | Ditto
type SSHCommand  = String
-- | Ditto
type RsyncPath   = String

-- |
-- State monad wrapper (should have used Read monad here, really).
--
-- Carries a three-tuple of Blog configuration and Blog interactions
-- are performed within this monad.
type BlogCfg = StateT (SSHCommand, (BlogCat -> RsyncPath), [BlogCat], [BlogTag]) IO

-- | Ditto
type Blog = BlogCfg (Either String Bool)

-- |
-- Get starting instance of BlogCfg (Either String Bool) from
-- arguments.
--
-- Resulting value will either Left with error message, or Right
-- with status of operations that were to be executed.
--
-- Consider Right False to mean "operations are still getting executed
-- or there was an unknown transport error". Letter is an exceptional
-- situation though.
withBlog :: SSHCommand -> (BlogCat -> RsyncPath) -> [BlogCat] -> [BlogTag] -> Blog
withBlog ssh rsyncF cs ts = state $ \_ -> (Right False, (ssh, rsyncF, cs, ts))

-- |
-- Simple withBlog version
withBlogSimple :: RsyncPath -> [BlogCat] -> [BlogTag] -> Blog
withBlogSimple rsyncPath cs ts = withBlog "ssh -p22" (defaultPathFunction rsyncPath) cs ts

-- |
-- Category -> Path function generator. Expects base rsync path to blog root
defaultPathFunction :: RsyncPath -> BlogCat -> RsyncPath
defaultPathFunction base "draft" = base ++ "drafts/"
defaultPathFunction base cat     = base ++  "posts/" ++ cat ++ "/"

post :: Blog -> String -> BlogCat -> [BlogTag] -> Blog
post x f pc pts = do
  (ssh, rsync, cs, ts) <- get
  postDo ssh rsync (pc `elem` cs) (foldl (\a t -> a && (t `elem` ts)) True pts)
  where
    postDo ssh rsync True True   = do
      io $ shell ("rsync -Pave \"" <> ssh <> "\" \"" <> pack f <> "\" " <> rsync pc) empty
      return $ Right True
    postDo _   _     False _     = do
      return $ Left "Unknown category"
    postDo _   _     _     False = do
      return $ Left "Unknown tag"

io :: IO a -> StateT [Integer] IO a
io = liftIO
