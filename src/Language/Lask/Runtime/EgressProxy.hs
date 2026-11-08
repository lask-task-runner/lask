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

-- | Published by the egress-proxy workflow from a3138df (run 42355073047),
-- for linux/amd64 and linux/arm64, signed with cosign.
embeddedProxyImage :: Maybe Text
embeddedProxyImage = Just "ghcr.io/lask-task-runner/lask-egress-proxy@sha256:97f1181305de62395aafe7ea5f450a0d10f2e6d1176577b00f6bbe0dc8791c4d"
