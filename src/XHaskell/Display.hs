module XHaskell.Display (
  display,
  Display(..),
  DisplayData(..)
) where

data DisplayData = DisplayData {
    mimeType :: String,
    content  :: String
}

instance Show DisplayData where
  -- 変数名は小文字(stx, us, etx)にし、where句を使うのが一般的です
  show (DisplayData m c) = stx ++ m ++ us ++ c ++ etx
    where
      stx = "\x02\&"
      us  = "\x1F\&"
      etx = "\x03"

class Display a where
  display :: a -> DisplayData
