{-# LANGUAGE OverloadedStrings #-}
module Core.Util.Prelude (prelude, preludeStr) where

import Data.Text as Text (concat)
import Control.Monad.Error
import Control.Monad.Identity

import Core.Parser
import Core.Types

preludeStr = Text.concat
   [ "I x = x                 ;\n"
   , "K x y = x               ;\n"
   , "K1 x y = y              ;\n"
   , "S f g x = f x (g x)     ;\n"
   , "compose f g x = f (g x) ;\n"
   , "twice f = compose f f   \n" ]

prelude = either error id $ parse "Core.Util.Prelude" preludeStr
