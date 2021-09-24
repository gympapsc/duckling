-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Quantity.DE.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Quantity.Helpers
import Duckling.Regex.Types (GroupMatch(..))
import Duckling.Types
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Quantity.Types (QuantityData(..))
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Quantity.Types as TQuantity

quantities :: [(Text, String, TQuantity.Unit)]
quantities =
  [ ("<quantity> cups", "(becher)", TQuantity.Cup)
  , ("<quantity> grams", "(((m(illi)?[.]?)|(k(ilo)?)[.]?)?g(ramm)?[.]?)[.]?", TQuantity.Gram)
  , ("<quantity> lb", "(pfund)", TQuantity.Pound)
  , ("<quantity> oz", "(unzen)", TQuantity.Ounce)
  ]

opsMap :: HashMap Text (Double -> Double)
opsMap = HashMap.fromList
  [ ( "milligramm", (/ 1000))
  , ( "mg"        , (/ 1000))
  , ( "m.g"       , (/ 1000))
  , ( "m.g."      , (/ 1000))
  , ( "kilogramm" , (* 1000))
  , ( "kg"        , (* 1000))
  , ( "k.g"       , (* 1000))
  , ( "k.g."      , (* 1000))
  ]

ruleNumeralQuantities :: [Rule]
ruleNumeralQuantities = map go quantities
  where
    go :: (Text, String, TQuantity.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern = [Predicate isPositive, regex regexPattern]
      , prod = \case
        (Token Numeral nd:
         Token RegexMatch (GroupMatch (match:_)):
         _) -> Just . Token Quantity $ quantity u value
          where value = getValue opsMap match $ TNumeral.value nd
        _ -> Nothing
      }

ruleAQuantity :: [Rule]
ruleAQuantity = map go quantities
  where
    go :: (Text, String, TQuantity.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern = [ regex ("ein " ++ regexPattern) ]
      , prod = \case
        (Token RegexMatch (GroupMatch (match:_)):
         _) -> Just . Token Quantity $ quantity u $ getValue opsMap match 1
        _ -> Nothing
      }

ruleQuantityOfProduct :: Rule
ruleQuantityOfProduct = Rule
  { name = "<quantity> von"
  , pattern =
    [ dimension Quantity
    , regex "von (\\w+)"
    ]
  , prod = \case
    (Token Quantity qd:Token RegexMatch (GroupMatch (product:_)):_) ->
      Just . Token Quantity $ withProduct (Text.toLower product) qd
    _ -> Nothing
  }

rulePrecision :: Rule
rulePrecision = Rule
    { name = "genau|exact <quantity>"
    , pattern =
      [ regex "\\~|exact|genau|ungefähr|etwa|nahe zu|um|fast"
      , dimension Quantity
      ]
      , prod = \case
        (_:token:_) -> Just token
        _ -> Nothing
  }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral = Rule
    { name = "zwischen|von <numeral> und|bis zu <quantity>"
    , pattern =
      [ regex "zwischen|von"
      , Predicate isPositive
      , regex "und|bis (zu)?"
      , Predicate isSimpleQuantity
      ]
    , prod = \case
        (_:
         Token Numeral NumeralData{TNumeral.value = from}:
         _:
         Token Quantity QuantityData{TQuantity.value = Just to
                                    , TQuantity.unit = Just u
                                    , TQuantity.aproduct = Nothing}:
         _) | from < to ->
          Just . Token Quantity . withInterval (from, to) $ unitOnly u
        _ -> Nothing
    }

ruleIntervalBetween :: Rule
ruleIntervalBetween = Rule
    { name = "zwischen|von <quantity> und|bis zu <quantity>"
    , pattern =
      [ regex "zwischen|von"
      , Predicate isSimpleQuantity
      , regex "und|bis (zu)?"
      , Predicate isSimpleQuantity
      ]
    , prod = \case
        (_:
         Token Quantity QuantityData{TQuantity.value = Just from
                                    , TQuantity.unit = Just u1
                                    , TQuantity.aproduct = Nothing}:
         _:
         Token Quantity QuantityData{TQuantity.value = Just to
                                    , TQuantity.unit = Just u2
                                    , TQuantity.aproduct = Nothing}:
         _) | from < to && u1 == u2 ->
          Just . Token Quantity . withInterval (from, to) $ unitOnly u1
        _ -> Nothing
    }

ruleIntervalNumeralDash :: Rule
ruleIntervalNumeralDash = Rule
    { name = "<numeral> - <quantity>"
    , pattern =
      [ Predicate isPositive
      , regex "\\-"
      , Predicate isSimpleQuantity
      ]
    , prod = \case
        (Token Numeral NumeralData{TNumeral.value = from}:
         _:
         Token Quantity QuantityData{TQuantity.value = Just to
                                    , TQuantity.unit = Just u
                                    , TQuantity.aproduct = Nothing}:
         _) | from < to ->
           Just . Token Quantity . withInterval (from, to) $ unitOnly u
        _ -> Nothing
    }

ruleIntervalDash :: Rule
ruleIntervalDash = Rule
    { name = "<quantity> - <quantity>"
    , pattern =
      [ Predicate isSimpleQuantity
      , regex "\\-"
      , Predicate isSimpleQuantity
      ]
    , prod = \case
        (Token Quantity QuantityData{TQuantity.value = Just from
                                    , TQuantity.unit = Just u1
                                    , TQuantity.aproduct = Nothing}:
         _:
         Token Quantity QuantityData{TQuantity.value = Just to
                                    , TQuantity.unit = Just u2
                                    , TQuantity.aproduct = Nothing}:
         _) | from < to && u1 == u2 ->
          Just . Token Quantity . withInterval (from, to) $ unitOnly u1
        _ -> Nothing
    }

ruleIntervalMax :: Rule
ruleIntervalMax = Rule
    { name = "unterhalb/weniger/maximal/nicht mehr als <dist>"
    , pattern =
      [ regex "unter(halb)?|weniger|maximal|(weniger|nicht mehr) als"
      , Predicate isSimpleQuantity
      ]
    , prod = \case
        (_:
         Token Quantity QuantityData{TQuantity.value = Just to
                                    , TQuantity.unit = Just u
                                    , TQuantity.aproduct = Nothing}:
         _) -> Just . Token Quantity . withMax to $ unitOnly u
        _ -> Nothing
    }

ruleIntervalMin :: Rule
ruleIntervalMin = Rule
  { name = "über/mehr/mindestens/mehr als <quantity>"
  , pattern =
      [ regex "über|oberhalb|mehr|mindestens|(mehr|größer|schwerer) als"
      , Predicate isSimpleQuantity
      ]
    , prod = \case
        (_:
         Token Quantity QuantityData{TQuantity.value = Just from
                                    , TQuantity.unit = Just u
                                    , TQuantity.aproduct = Nothing}:
         _) -> Just . Token Quantity . withMin from $ unitOnly u
        _ -> Nothing
    }

ruleQuantityLatent :: Rule
ruleQuantityLatent = Rule
  { name = "<quantity> (latent)"
  , pattern =
    [ Predicate isPositive
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}: _) ->
        Just $ (Token Quantity . mkLatent) $ valueOnly v
      _ -> Nothing
  }


rules :: [Rule]
rules =
  [ ruleQuantityOfProduct
  , ruleIntervalMin
  , ruleIntervalMax
  , ruleIntervalBetweenNumeral
  , ruleIntervalBetween
  , ruleIntervalNumeralDash
  , ruleIntervalDash
  , rulePrecision
  , ruleQuantityLatent
  ]
  ++ ruleNumeralQuantities
  ++ ruleAQuantity
