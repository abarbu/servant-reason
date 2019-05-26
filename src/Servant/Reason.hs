{-|
Basic usage:

> import MyLib (MyServantApiType)
> import Servant.Reason
>
> spec :: Spec
> spec = Spec ["Generated", "MyApi"]
>             (defReasonImports : generateReasonForAPI (Proxy :: Proxy MyServantApiType))
>
> main :: IO ()
> main = specsToDir [spec] "my-reason-dir"
-}
module Servant.Reason
       ( generateReasonForAPI
       , generateReasonForAPIWith
       , ReasonOptions(..)
       , UrlPrefix(..)
       , defReasonOptions
       , defReasonImports
       -- * Convenience re-exports from the "Reason" module
       , Spec(Spec)
       , ReasonType
       , specsToDir
       -- * Convenience re-exports from "Data.Proxy"
       , Proxy(Proxy)
       ) where

import           Servant.Reason.Internal.Generate (ReasonOptions (..), UrlPrefix (..),
                                                   defReasonImports, defReasonOptions,
                                                   generateReasonForAPI,
                                                   generateReasonForAPIWith)

import           Data.Proxy                    (Proxy (Proxy))
import           Reason                        (ReasonType, Spec (Spec),
                                                specsToDir)
