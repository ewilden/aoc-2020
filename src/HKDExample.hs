{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HKDExample where

import Barbies
import Import
import qualified RIO.HashSet as HashSet

-- For concepts, see https://reasonablypolymorphic.com/blog/higher-kinded-data/
-- For library, see https://hackage.haskell.org/package/barbies-2.0.2.0/docs/Barbies.html
-- To avoid some boilerplate, see barbies-th https://hackage.haskell.org/package/barbies-th-0.1.7/docs/Barbies-TH.html

data Field' f
  = X (f (Int, Int))
  | Y (f (Int, Text, Bool))
  | Z (f ())
  deriving
    ( Generic,
      FunctorB,
      TraversableB,
      ConstraintsB
    )

deriving instance AllBF Eq f Field' => Eq (Field' f)

deriving instance AllBF Show f Field' => Show (Field' f)

instance Hashable (Field' (Const ()))

type Field = Field' Identity

type FieldType = Field' (Const ())

fieldType :: Field -> FieldType
fieldType = bmap (const $ Const ())

getFieldSet :: [Field] -> HashSet FieldType
getFieldSet = HashSet.fromList . map fieldType

matchOnFieldType :: FieldType -> Text
matchOnFieldType fieldType = case fieldType of
  X _ -> "This is an X"
  Y (Const ()) -> "This is a Y, note how there's no longer any data in it"
  Y _ -> "And therefore this pattern match is redundant"
  _ -> "This is something else"
