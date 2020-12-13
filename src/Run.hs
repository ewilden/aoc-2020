{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Day13 (answer2, input)
import Import

run :: RIO App ()
run = do
  n <- liftIO $ answer2 <$> input
  logInfo $ displayShow n
