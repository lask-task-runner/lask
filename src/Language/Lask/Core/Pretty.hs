{-# LANGUAGE OverloadedStrings #-}

-- | Render Core in Lask-like notation, for tests and examples that show
-- what a program lowers to. It prints Core, not sugar: @$ cmd@ appears
-- as the @run@ and @if@ it lowers to. Nodes without a rendering print
-- as @…@.
module Language.Lask.Core.Pretty (renderDecl, renderCore) where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Core.AST
import Language.Lask.Elaborate (CoreDecl (..))
import Language.Lask.Runtime.Value (formatNumber)
import Language.Lask.Types (Type (..), renderType)

-- | A declaration as @name(params): Type =@ and its body.
renderDecl :: CoreDecl -> Text
renderDecl cd = case coreF (cdCore cd) of
  CLam lam ->
    cdName cd <> "(" <> T.intercalate ", " (params lam) <> "): " <> ret (lamType lam) <> " =\n  " <> renderCore 1 (lamBody lam)
  _ -> cdName cd <> " = " <> renderCore 1 (cdCore cd)
  where
    params lam =
      zipWith (\n t -> n <> ": " <> renderType t) (lamPositional lam) (argTys (lamType lam))
        <> ["--" <> n <> " = " <> renderCore 1 d | (n, d) <- lamKeywords lam]
    argTys (TyFun as _) = as
    argTys _ = []
    ret (TyFun _ r) = renderType r
    ret t = renderType t

-- | An expression, at the given indentation depth.
renderCore :: Int -> Core -> Text
renderCore ind c = case coreF c of
  CNull -> "null"
  CBool b -> if b then "true" else "false"
  CNumber n -> formatNumber n
  CStrLit t -> "\"" <> escape t <> "\""
  CStr ps -> "\"" <> T.concat (map part ps) <> "\""
  CVar (LocalRef n) -> n
  CVar (TopRef _ n) -> n
  CVar (BuiltinRef n) -> n
  CArray es -> "[" <> T.intercalate ", " (map go es) <> "]"
  CRecordLit kvs -> "{" <> T.intercalate ", " [k <> ": " <> go v | (k, v) <- kvs] <> "}"
  CMapLit kvs -> "{" <> T.intercalate ", " [k <> ": " <> go v | (k, v) <- kvs] <> "}"
  CLam lam -> "\\(" <> T.intercalate ", " (lamPositional lam) <> ") -> " <> go (lamBody lam)
  CApp f pos kws -> go f <> "(" <> T.intercalate ", " (map go pos <> ["--" <> k <> " = " <> go v | (k, v) <- kws]) <> ")"
  CDot e f -> go e <> "." <> f
  CIf a b e -> "if (" <> bare a <> ") " <> block b <> " else " <> block e
  CAnd a b -> "(" <> go a <> " && " <> go b <> ")"
  COr a b -> "(" <> go a <> " || " <> go b <> ")"
  CNot a -> "!" <> go a
  CBin op a b -> "(" <> go a <> " " <> sym op <> " " <> go b <> ")"
  CDo [] -> "{}"
  CDo ss -> "do {\n" <> T.concat [pad (ind + 1) <> stmt s <> "\n" | s <- ss] <> pad ind <> "}"
  CAwait a -> "await " <> go a
  CEnv "local" _ -> "#local"
  CEnv _ args | Just (Core _ (CStrLit i)) <- lookup "image" args -> "#" <> i
  _ -> "…"
  where
    go = renderCore ind
    bare x = case coreF x of
      CBin op a b -> go a <> " " <> sym op <> " " <> go b
      _ -> go x
    block x = case coreF x of
      CDo _ -> renderCore ind x
      _ -> "{ " <> go x <> " }"
    stmt (CSBind n e) = n <> " = " <> renderCore (ind + 1) e
    stmt (CSExpr e) = renderCore (ind + 1) e
    part (CPText t) = escape t
    part (CPExpr e) = "#{" <> go e <> "}"
    escape = T.replace "\n" "\\n" . T.replace "\"" "\\\""
    pad n = T.replicate (2 * n) " "
    sym op = case op of
      PAdd -> "+"
      PSub -> "-"
      PMul -> "*"
      PDiv -> "/"
      PEq -> "=="
      PNe -> "!="
      PLt -> "<"
      PLe -> "<="
      PGt -> ">"
      PGe -> ">="
