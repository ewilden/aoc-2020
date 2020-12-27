{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Day20 (answer2, input)
import Import

run :: RIO App ()
run = do
  logInfo $ "Answering..."
  liftIO answer2 >>= (logInfo . displayShow)
