module Main (main) where

import           Data.Map           (fromList)
import           System.Environment (getArgs)

import           Blog

c :: TextText
c = fromList [ ("Cmd",    "ssh -p 21984")
             , ("Remote", "memorici.de")
             , ("Path",   "github/lue") ]

b :: TextTexts
b = fromList [ ("BlogCat", ["draft", "ru", "life", "tech"]) ]

f :: BlogCat -> Path -> Path -> Path
f "tech" = defaultPathFn "universe"
f x      = defaultPathFn x

cfg :: BlogCfg
cfg = BlogCfg { connection = c, blog_spec = b, pathFn = f }

main :: IO Blog
main = do
  [file, cat]  <- getArgs
  lift $ post cfg file cat
