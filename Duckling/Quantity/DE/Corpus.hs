-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.DE.Corpus
  ( corpus
  , latentCorpus
  ) where

import Prelude
import Data.String

import Duckling.Quantity.Types
import Duckling.Resolve (Options(..))
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)


latentCorpus :: Corpus
latentCorpus = (testContext, testOptions {withLatent = True}, latentExamples)
  where
    latentExamples = concat
      [
      examples (simple Unnamed 4 Nothing)
                [ "ungefähr 4"
                , "vier"
                , "~ vier"
                ]
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (simple Gram 500 (Just "Erdbeeren"))
        [ "500 gramm Erdbeeren"
        , "500g Erdbeeren"
        , "0.5 Kilogramm Erdbeeren"
        , "0.5 Kg Erdbeeren"
        , "500000mg Erdbeeren"
        ]
  ]
