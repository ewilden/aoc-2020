{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Day15 (answer2, input)
import Import

run :: RIO App ()
run = do
  logInfo $ "Answering..."
  logInfo $ displayShow (answer2)
