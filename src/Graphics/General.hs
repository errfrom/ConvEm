module Graphics.General where

import Data.Text (Text, unpack)

type Id    = Text
type Class = Text

selNonexistent :: Text -> IO ()
selNonexistent sel = putStrLn $ "Not valid selector - " ++ (unpack sel) ++ "."
