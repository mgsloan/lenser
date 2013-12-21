{-# LANGUAGE TemplateHaskell #-}

module Main where

import Lenser
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Instances
import Control.Lens

--FIXME: Something is wrong with multi-field constructors

data Test
  = Test { test :: String }
  deriving Show

data X
  = X { x :: Int }
  | Y { y :: Int }
  | Z
  deriving Show

$(lenser[d|
    _test = lensFor test
    _y = traversalFor [x, y]
  |])

main = do
  print (Test "hello" & _test .~ "world")
  print (Y 1 & _y .~ 3)
