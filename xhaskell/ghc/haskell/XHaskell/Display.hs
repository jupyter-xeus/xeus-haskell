module XHaskell.Display (
  Display(..),
  DisplayData(..)
) where

data DisplayData = DisplayData {
    mimeType :: String,
    content  :: String
}

instance Show DisplayData where
  show (DisplayData mime contentValue) =
    "\x02" ++ mime ++ "\x1f" ++ contentValue ++ "\x03"

class Display a where
  display :: a -> DisplayData
