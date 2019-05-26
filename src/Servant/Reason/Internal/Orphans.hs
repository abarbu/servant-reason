{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Reason.Internal.Orphans where

import           Reason         (ReasonDatatype, ReasonType, toReasonType)
import           Servant.API (NoContent, Headers, getResponse)


instance ReasonType ReasonDatatype where
  toReasonType = id


instance ReasonType NoContent


-- TODO: Generate Reason functions that can handle the response headers. PRs
-- welcome!
instance (ReasonType a) => ReasonType (Headers ls a) where
  toReasonType = toReasonType . getResponse
