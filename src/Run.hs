{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Day23 (answer2')
import Import

run :: RIO App ()
run = do
  logInfo $ "Answering..."
  logInfo $ displayShow $ answer2'
