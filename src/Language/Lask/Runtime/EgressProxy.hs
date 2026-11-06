{-# LANGUAGE OverloadedStrings #-}

-- | The egress proxy image this release of Lask runs (spec 10.12): a
-- registry reference with its digest, built from @egress-proxy/@ and
-- published by the @egress-proxy@ workflow. Each release names one,
-- so that the proxy a container goes through is fixed by the Lask that
-- starts it.
module Language.Lask.Runtime.EgressProxy
  ( embeddedProxyImage,
  )
where

import Data.Text (Text)

-- | 'Nothing' until the first image is published. A build without one
-- runs @egress@ only with @LASK_EGRESS_PROXY_IMAGE@ set.
embeddedProxyImage :: Maybe Text
embeddedProxyImage = Nothing
