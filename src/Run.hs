{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Day10 (answer2, input)
import Import

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  n <- liftIO $ answer2 <$> input
  logInfo $ displayShow n
