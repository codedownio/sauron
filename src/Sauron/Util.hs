{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Sauron.Util where

import Data.Either
import qualified Graphics.Vty as V
import UnliftIO.Exception


isTuiSupported :: IO Bool
isTuiSupported = isRight <$> tryAny (V.mkVty V.defaultConfig)
