{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Servant.Reason.Internal.Foreign where

import           Data.Proxy      (Proxy (Proxy))
import           Reason             (ReasonDatatype, ReasonType, toReasonType)
import           Servant.Foreign (Foreign, GenerateList, HasForeign,
                                  HasForeignType, Req, listFromAPI, typeFor)


data LangReason

instance (ReasonType a) => HasForeignType LangReason ReasonDatatype a where
  typeFor _ _ _ =
    toReasonType (Proxy :: Proxy a)

getEndpoints
  :: ( HasForeign LangReason ReasonDatatype api
     , GenerateList ReasonDatatype (Foreign ReasonDatatype api))
  => Proxy api
  -> [Req ReasonDatatype]
getEndpoints =
  listFromAPI (Proxy :: Proxy LangReason) (Proxy :: Proxy ReasonDatatype)
