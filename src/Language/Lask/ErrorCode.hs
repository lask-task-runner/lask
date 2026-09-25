{-# LANGUAGE OverloadedStrings #-}

-- | Error codes and stages defined by spec chapter 14.
module Language.Lask.ErrorCode
  ( ErrorCode (..),
    Stage (..),
    codeText,
    stageText,
  )
where

import Data.Text (Text)

-- | @E-\<CATEGORY\>-\<DETAIL\>@ codes (spec 14.2, 14.4-14.6).
data ErrorCode
  = ESyntaxUnexpectedToken
  | ESyntaxReturnPosition
  | ESyntaxCaseElse
  | ENameUndefined
  | ENameAmbiguous
  | ENameDuplicate
  | ETypeMismatch
  | ETypeArity
  | ETypeCall
  | ETypeCommandEnv
  | ETypeCommandNoEnv
  | ETypeCommandConflict
  | ETypeCommandEffect
  | ETypeCommandName
  | ETypeCommandDuplicate
  | ETypeEnvConstruct
  | ETypeAccess
  | ETypeFieldDuplicate
  | ETypeCaseDuplicate
  | ETypeKeyword
  | ETypeIllformed
  | ETypeSecretNonString
  | EModuleCycle
  | EModuleUnresolved
  | EModuleDeepImport
  | EModuleLockStale
  | EModuleRevMoved
  | EModuleHashMismatch
  | ERuntimeDivByZero
  | ERuntimeCommandNonzero
  | ERuntimeAwaitFailed
  | ERuntimeAccess
  | ERuntimeCast
  | ERuntimeValue
  | ERuntimeRegex
  | EIoStdinRead
  | EIoEnvResolve
  | EIoImageMissing
  | EIoImageDigest
  | EIoFs
  | EIoDataDecode
  | ECliUsage
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Error stage (spec 14.3).
data Stage
  = StageSyntax
  | StageStatic
  | StageRuntime
  | StageIo
  | StageCli
  deriving (Show, Eq, Ord, Enum, Bounded)

codeText :: ErrorCode -> Text
codeText c = case c of
  ESyntaxUnexpectedToken -> "E-SYNTAX-UNEXPECTED-TOKEN"
  ESyntaxReturnPosition -> "E-SYNTAX-RETURN-POSITION"
  ESyntaxCaseElse -> "E-SYNTAX-CASE-ELSE"
  ENameUndefined -> "E-NAME-UNDEFINED"
  ENameAmbiguous -> "E-NAME-AMBIGUOUS"
  ENameDuplicate -> "E-NAME-DUPLICATE"
  ETypeMismatch -> "E-TYPE-MISMATCH"
  ETypeArity -> "E-TYPE-ARITY"
  ETypeCall -> "E-TYPE-CALL"
  ETypeCommandEnv -> "E-TYPE-COMMAND-ENV"
  ETypeCommandNoEnv -> "E-TYPE-COMMAND-NOENV"
  ETypeCommandConflict -> "E-TYPE-COMMAND-CONFLICT"
  ETypeCommandEffect -> "E-TYPE-COMMAND-EFFECT"
  ETypeCommandName -> "E-TYPE-COMMAND-NAME"
  ETypeCommandDuplicate -> "E-TYPE-COMMAND-DUPLICATE"
  ETypeEnvConstruct -> "E-TYPE-ENV-CONSTRUCT"
  ETypeAccess -> "E-TYPE-ACCESS"
  ETypeFieldDuplicate -> "E-TYPE-FIELD-DUPLICATE"
  ETypeCaseDuplicate -> "E-TYPE-CASE-DUPLICATE"
  ETypeKeyword -> "E-TYPE-KEYWORD"
  ETypeIllformed -> "E-TYPE-ILLFORMED"
  ETypeSecretNonString -> "E-TYPE-SECRET-NON-STRING"
  EModuleCycle -> "E-MODULE-CYCLE"
  EModuleUnresolved -> "E-MODULE-UNRESOLVED"
  EModuleDeepImport -> "E-MODULE-DEEP-IMPORT"
  EModuleLockStale -> "E-MODULE-LOCK-STALE"
  EModuleRevMoved -> "E-MODULE-REV-MOVED"
  EModuleHashMismatch -> "E-MODULE-HASH-MISMATCH"
  ERuntimeDivByZero -> "E-RUNTIME-DIV-BY-ZERO"
  ERuntimeCommandNonzero -> "E-RUNTIME-COMMAND-NONZERO"
  ERuntimeAwaitFailed -> "E-RUNTIME-AWAIT-FAILED"
  ERuntimeAccess -> "E-RUNTIME-ACCESS"
  ERuntimeCast -> "E-RUNTIME-CAST"
  ERuntimeValue -> "E-RUNTIME-VALUE"
  ERuntimeRegex -> "E-RUNTIME-REGEX"
  EIoStdinRead -> "E-IO-STDIN-READ"
  EIoEnvResolve -> "E-IO-ENV-RESOLVE"
  EIoImageMissing -> "E-IO-IMAGE-MISSING"
  EIoImageDigest -> "E-IO-IMAGE-DIGEST"
  EIoFs -> "E-IO-FS"
  EIoDataDecode -> "E-IO-DATA-DECODE"
  ECliUsage -> "E-CLI-USAGE"

stageText :: Stage -> Text
stageText s = case s of
  StageSyntax -> "syntax"
  StageStatic -> "static"
  StageRuntime -> "runtime"
  StageIo -> "io"
  StageCli -> "cli"
