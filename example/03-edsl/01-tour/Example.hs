{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeOperators #-}

-- | A Lask program written with "Language.Lask.Embed". Above each
-- declaration is the Lask source it corresponds to.
module Example
  ( go,
    npm,
    git,
    kubectl,
    testApi,
    testWeb,
    test,
    fact,
    changed,
    lintChanged,
    release,
    ship,
    goTest,
    testEach,
    deploy,
    program,
  )
where

import Data.Function ((&))
import Data.Text (Text)
import Language.Lask.Embed
import qualified Language.Lask.Embed.Do as L

-- Command words -----------------------------------------------------------------
--
--   command { "go" }      on #golang:1.22
--   command { "npm" }     on #node:20
--   command { "git" }     on #alpine/git:2.45.2
--   command { "kubectl" } on #bitnami/kubectl:1.30

go, npm, git, kubectl :: Command
go = command "go" (image "golang:1.22")
npm = command "npm" (image "node:20")
git = command "git" (image "alpine/git:2.45.2")
kubectl = command "kubectl" (image "bitnami/kubectl:1.30")

-- 1. Plain tasks ------------------------------------------------------------------
--
--   test_api(): String = $ go test ./...
--   test_web(): String = $ npm test

testApi :: Task '[] 'TString
testApi = task "test_api" $ run go "test ./..."

testWeb :: Task '[] 'TString
testWeb = task "test_web" $ run npm "test"

-- 2. Parallel composition is an applicative ---------------------------------------
--
--   # Run the API and web test suites concurrently
--   test(): String = do {
--     api = async test_api()
--     web = async test_web()
--     a = await api
--     w = await web
--     "#{a}\n#{w}"
--   }
--
-- Neither job can see the other's result: that is what <*> guarantees, and
-- why both can be spawned before either is awaited.

test :: Task '[] 'TString
test =
  doc "Run the API and web test suites concurrently" $
    task "test" $
      parallel $
        (\api web -> api <> "\n" <> web)
          <$> par (call testApi)
          <*> par (call testWeb)

-- 3. Recursion with a run-time base case -------------------------------------------
--
--   fact(n: Number): Number = if (n == 0) { 1 } else { n * fact(n - 1) }
--
-- `call fact` refers to fact by name and never copies its body, so this
-- Haskell-level recursion builds a finite term.

fact :: Task '["n" ::: 'TNumber] 'TNumber
fact = task "fact" $ \n ->
  if_ (n ==. 0) 1 (n * call fact (n - 1))

-- 4. Effects that depend on earlier results ----------------------------------------
--
--   changed(): Array<String> = do {
--     out = $ git diff --name-only origin/main
--     lines(out)
--   }
--
--   lint_changed(): Void = for_each(changed(), \(f) -> do {
--     $ npm exec eslint -- #{f}
--   })
--
-- Which commands run depends on what an earlier command printed, which an
-- applicative could not express. The term is still static: `envs` lists
-- node:20 and alpine/git for lint_changed without running anything.

changed :: Task '[] ('TArray 'TString)
changed = task "changed" $ L.do
  out <- run git "diff --name-only origin/main"
  lines_ out

lintChanged :: Task '[] 'TVoid
lintChanged = task "lint_changed" $
  forEach (call changed) $ \f ->
    run npm ("exec eslint -- " <> f)

-- 5. Keyword parameters, and sharing with <- ----------------------------------------
--
--   # Publish the latest tag to a target
--   release(target: String, --dry_run: Bool = true): String = do {
--     tag = $ git describe --tags --abbrev=0
--     if (dry_run) {
--       "would publish #{tag} to #{target}"
--     } else {
--       $ go run ./cmd/publish --tag #{tag} --target #{target}
--       "published #{tag} to #{target}"
--     }
--   }
--
-- `tag` is bound with <-, so `git describe` runs once although `tag` is
-- used three times. A Haskell `let tag = ...` would copy the command into
-- each use instead.

release :: Task '["target" ::: 'TString] 'TString
release =
  doc "Publish the latest tag to a target" $
    taskWith "release" $
      body <$> (kw "dry_run" true & help "Only print what would be published")
  where
    body dryRun target = L.do
      tag <- run git "describe --tags --abbrev=0"
      if_
        dryRun
        ("would publish " <> tag <> " to " <> target)
        ( L.do
            run go ("run ./cmd/publish --tag " <> tag <> " --target " <> target)
            "published " <> tag <> " to " <> target
        )

-- 6. A keyword argument ------------------------------------------------------------
--
--   ship(): String = release("prod", dry_run = false)

ship :: Task '[] 'TString
ship = task "ship" $ callWith release ["dry_run" .= false] "prod"

-- 7. Macros: plain Haskell functions over E -------------------------------------------
--
-- A helper like this is inlined at each use. It has no name, is not in the
-- call graph, and cannot recurse on a run-time value; use `task` for that.

goTest :: E v 'TString -> E v 'TString
goTest pkg = run go ("test -count=1 " <> pkg)

--   test_each(): Array<String> = do {
--     pkgs = $ go list ./...
--     for (pkg : lines(pkgs)) { $ go test -count=1 #{pkg} }
--   }

testEach :: Task '[] ('TArray 'TString)
testEach = task "test_each" $ L.do
  pkgs <- run go "list ./..."
  mapE (lines_ pkgs) goTest

-- 8. Environment variables -------------------------------------------------------------
--
--   deploy(): String =
--     $ kubectl apply -f k8s/ --context #{get_env_or("KUBE_CONTEXT", "staging")}

deploy :: Task '[] 'TString
deploy = task "deploy" $
  run kubectl ("apply -f k8s/ --context " <> getEnvOr "KUBE_CONTEXT" "staging")

-- The program ---------------------------------------------------------------------------
--
-- The exports are what `lask run` and `--help` see. `changed`, `test_api`
-- and `test_web` are reached through calls and pulled in; `fact` is
-- internal.

program :: Either [Text] Program
program =
  assemble
    [ export test,
      export lintChanged,
      export release,
      export ship,
      export testEach,
      export deploy,
      internal fact
    ]
