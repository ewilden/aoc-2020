{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Day11 (answer1, input)
import Import

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  n <- liftIO $ answer1 <$> input
  logInfo $ displayShow n
