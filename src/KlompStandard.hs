{-# LANGUAGE DeriveDataTypeable #-}
module KlompStandard where

import qualified Data.Typeable as T
import qualified Data.Data as TD


ccEq :: (TD.Data a) => a -> TD.Constr -> Bool
ccEq e c = TD.toConstr e == c


{-
class CC a where
  cc :: a -> TD.Constr

instance TD.Data a => CC a where
  cc = TD.toConstr

instance CC a => CC (b -> a) where
  cc f = cc (f undefined)
-}
