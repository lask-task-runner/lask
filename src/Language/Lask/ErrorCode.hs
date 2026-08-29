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
  | ENameUndefined
  | ENameAmbiguous
  | ENameDuplicate
  | ETypeMismatch
  | ETypeArity
  | ETypeCall
  | ETypeCommandEnv
  | ETypeEnvConstruct
  | ETypeAccess
  | ETypeFieldDuplicate
  | ETypeKeyword
  | ETypeIllformed
  | ETypeSecretNonString
  | EModuleCycle
  | EModuleUnresolved
  | EModuleHashMismatch
  | ERuntimeDivByZero
  | ERuntimeCommandNonzero
  | ERuntimeAwaitFailed
  | ERuntimeAccess
  | ERuntimeCast
  | EIoStdinRead
  | EIoSshConnect
  | EIoSshAuth
  | EIoEnvResolve
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
  ENameUndefined -> "E-NAME-UNDEFINED"
  ENameAmbiguous -> "E-NAME-AMBIGUOUS"
  ENameDuplicate -> "E-NAME-DUPLICATE"
  ETypeMismatch -> "E-TYPE-MISMATCH"
  ETypeArity -> "E-TYPE-ARITY"
  ETypeCall -> "E-TYPE-CALL"
  ETypeCommandEnv -> "E-TYPE-COMMAND-ENV"
  ETypeEnvConstruct -> "E-TYPE-ENV-CONSTRUCT"
  ETypeAccess -> "E-TYPE-ACCESS"
  ETypeFieldDuplicate -> "E-TYPE-FIELD-DUPLICATE"
  ETypeKeyword -> "E-TYPE-KEYWORD"
  ETypeIllformed -> "E-TYPE-ILLFORMED"
  ETypeSecretNonString -> "E-TYPE-SECRET-NON-STRING"
  EModuleCycle -> "E-MODULE-CYCLE"
  EModuleUnresolved -> "E-MODULE-UNRESOLVED"
  EModuleHashMismatch -> "E-MODULE-HASH-MISMATCH"
  ERuntimeDivByZero -> "E-RUNTIME-DIV-BY-ZERO"
  ERuntimeCommandNonzero -> "E-RUNTIME-COMMAND-NONZERO"
  ERuntimeAwaitFailed -> "E-RUNTIME-AWAIT-FAILED"
  ERuntimeAccess -> "E-RUNTIME-ACCESS"
  ERuntimeCast -> "E-RUNTIME-CAST"
  EIoStdinRead -> "E-IO-STDIN-READ"
  EIoSshConnect -> "E-IO-SSH-CONNECT"
  EIoSshAuth -> "E-IO-SSH-AUTH"
  EIoEnvResolve -> "E-IO-ENV-RESOLVE"
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
