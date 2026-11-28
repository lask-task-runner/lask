{-# LANGUAGE OverloadedStrings #-}

-- | Commands and the environments they run in (spec 16.5, 16.7).
module Cli.CommandsSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Foldable (toList)
import Data.List (isInfixOf, isPrefixOf, nub, sort)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Support.Harness
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = beforeAll findLask $ do
  describe "commands and environments (spec 16.5, 16.7)" $ do
    it "runs local commands with interpolation" $ \lask ->
      withProject [("main.lask", "n = \"world\"\nf() = $[#local] echo hello #{n}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "f"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "\"hello world\\n\"\n"
    it "envs lists referenced environments" $ \lask ->
      withProject
        [ ("main.lask", "f() = $[#./infra/Dockerfile] make\ng() = $[#alpine:3.20] ls\n")
        ]
        $ \dir -> do
          r <- runLask lask dir ["envs", "list"] ""
          resExit r `shouldBe` 0
          resOut r `shouldSatisfy` isInfixOf "infra/Dockerfile"
          resOut r `shouldSatisfy` isInfixOf "alpine:3.20"
    it "limits envs to the call graph of the given function (spec 11.4)" $ \lask ->
      withProject
        [ ( "main.lask",
            "build_go() = $[#golang:1.22] go build ./...\n\
            \build_node() = $[#node:20] npm run build\n\
            \backend() = build_go()\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["envs", "list", "backend"] ""
          resExit r `shouldBe` 0
          resOut r `shouldSatisfy` isInfixOf "golang:1.22"
          resOut r `shouldNotContain` "node:20"
          whole <- runLask lask dir ["envs", "list"] ""
          resOut whole `shouldSatisfy` isInfixOf "node:20"
    it "lists the programs run in each environment (spec 11.4)" $ \lask ->
      withProject
        [ ( "main.lask",
            "command { \"go\" } on golang\n\
            \golang = #golang:1.22\n\
            \envs: Map<Environment> = { \"a\": #alpine:3.20, \"b\": #debian:13 }\n\
            \build() = $ go build ./...\n\
            \clean() = $[#local] rm -rf dist && { echo done >&2; }\n\
            \pick(--env: String = \"a\") = $[envs[env]] make\n\
            \fixed() = $[envs[\"b\"]] ls\n\
            \on(image: Environment) = $[image] cat x\n\
            \with(--image: Environment = #busybox:1.37) = $[image] wc -l x\n\
            \needs(--image: Environment) = $[image] head x\n\
            \session() = with_container(#alpine:3.20, \\(box) -> do {\n\
            \  $[box] tar xf x\n\
            \})\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["envs", "commands", "--output", "json"] ""
          resExit r `shouldBe` 0
          let envs = maybe [] id (A.decode (BL8.pack (resOut r)) :: Maybe [A.Value])
              programsOf e =
                sort
                  [ p
                  | A.Object o <- envs,
                    KM.lookup "environment" o == Just (A.String e),
                    Just (A.Array ps) <- [KM.lookup "programs" o],
                    A.Object po <- toList ps,
                    Just (A.String p) <- [KM.lookup "program" po]
                  ]
          programsOf "#local" `shouldBe` ["echo", "rm"]
          programsOf "#golang:1.22" `shouldBe` ["go"]
          -- A session runs in the environment it was opened on.
          programsOf "#alpine:3.20" `shouldBe` ["make", "tar"]
          programsOf "#debian:13" `shouldBe` ["ls", "make"]
          programsOf "#busybox:1.37" `shouldBe` ["wc"]
          -- A required keyword parameter has no default to follow.
          programsOf "(dynamic)" `shouldBe` ["cat", "head", "wc"]
          text <- runLask lask dir ["envs", "commands"] ""
          resOut text `shouldSatisfy` isInfixOf "main.lask: pick (envs[env])"
          resOut text `shouldSatisfy` isInfixOf "5 environments and (dynamic): 9 programs in 8 commands"
          verbose <- runLask lask dir ["envs", "commands", "--verbose", "build"] ""
          resExit verbose `shouldBe` 0
          resOut verbose `shouldSatisfy` isInfixOf "main.lask:4  build  go build ./...  ← command \"go\""
    it "lists what each environment's commands may reach past the container (spec 11.4)" $ \lask ->
      withProject
        [ ( "main.lask",
            "host = #local\n\
            \alpine = #alpine:3.20\n\
            \sealed = #alpine:3.20{network: \"none\"}\n\
            \node = #node:24{base_access: { \".\": \"ro\", \"dist\": \"rw\" }, network: \"none\", read_only: true}\n\
            \deploy() = $[host] echo deploy\n\
            \build(): String = do {\n\
            \  $[node] npm run build\n\
            \  read_file(\"dist/out.json\", node)\n\
            \}\n\
            \release_of(e: Runnable): String = $[e] cat /etc/alpine-release\n\
            \ok(): String = release_of(sealed)\n\
            \bad(): String = release_of(alpine)\n\
            \ship() = $[#docker:27{volumes: [\"/var/run/docker.sock:/var/run/docker.sock\"]}] docker ps\n\
            \serve() = $[#nginx:1.27{publish: [\"8080:80\"]}] nginx\n\
            \probe() = $[runnable(alpine, network = get_env(\"NET\"))] true\n\
            \moved() = $[#alpine:3.20{base_mount: \"/src\"}] pwd\n"
          )
        ]
        $ \dir -> do
          files <- runLask lask dir ["envs", "files", "--verbose", "build"] ""
          resExit files `shouldBe` 0
          resOut files `shouldSatisfy` isInfixOf "#node:24{base_access,read_only}"
          resOut files `shouldSatisfy` isInfixOf "/work/dist"
          resOut files `shouldSatisfy` isInfixOf "main.lask:8  build  read_file  dist/out.json"
          resOut files `shouldSatisfy` isInfixOf "1 row (1 registry): 1 unconstrained"
          whole <- runLask lask dir ["envs", "files"] ""
          resOut whole `shouldSatisfy` isInfixOf "/var/run/docker.sock  rw !"
          resOut whole `shouldSatisfy` isInfixOf "(host)"
          resOut whole `shouldSatisfy` isInfixOf "/src "
          network <- runLask lask dir ["envs", "network"] ""
          resExit network `shouldBe` 0
          -- An environment a caller passes is listed at that caller.
          resOut network `shouldSatisfy` isInfixOf "main.lask: ok → release_of"
          resOut network `shouldSatisfy` isInfixOf "main.lask: bad → release_of"
          resOut network `shouldSatisfy` isInfixOf "(computed)"
          resOut network `shouldSatisfy` isInfixOf "8080:80 !"
          json <- runLask lask dir ["envs", "access", "--output", "json"] ""
          resExit json `shouldBe` 0
          -- REQUIRED BY holds an arrow, so the output is read as UTF-8.
          let rows = maybe [] id (A.decode (TLE.encodeUtf8 (TL.pack (resOut json))) :: Maybe [A.Value])
              classOf e =
                [ c
                | A.Object o <- rows,
                  KM.lookup "environment" o == Just (A.String e),
                  Just (A.Object n) <- [KM.lookup "network" o],
                  Just (A.String c) <- [KM.lookup "class" n]
                ]
          classOf "#local" `shouldBe` ["host"]
          classOf "#node:24{base_access,network}" `shouldBe` ["none"]
          sort (classOf "#alpine:3.20{network}") `shouldBe` ["computed", "none"]
          classOf "(dynamic)" `shouldBe` ["dynamic"]
    it "checks access constraints statically, at the call that resolves a parameter (spec 10.11)" $ \lask ->
      withProject
        [ ( "main.lask",
            "within { volumes: false }\n\
            \alpine = #alpine:3.20\n\
            \sealed = #alpine:3.20{network: \"none\"}\n\
            \release_of(e: Runnable): String within { outbound: false } = $[e] cat /etc/alpine-release\n\
            \ok(): String = release_of(sealed)\n\
            \bad(): String = release_of(alpine)\n\
            \probe() within { outbound: false } = $[runnable(alpine, network = get_env(\"NET\"))] true\n\
            \host() within { files: \"ro\" } = $[#local] ls\n\
            \ship() = $[#docker:27{volumes: [\"/var/run/docker.sock:/var/run/docker.sock\"]}] docker ps\n\
            \twice(f: Function<String>): String within { outbound: false } = f()\n\
            \pick = release_of\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["check"] ""
          resExit r `shouldBe` 1
          let out = resOut r
          -- The call that passes the environment is where it is reported.
          out `shouldSatisfy` isInfixOf "main.lask:6:17-6:35: E-TYPE-WITHIN-EXCEEDED"
          out `shouldSatisfy` isInfixOf "reached through bad → release_of"
          out `shouldNotSatisfy` isInfixOf "main.lask:5:"
          out `shouldSatisfy` isInfixOf "main.lask:7:"
          out `shouldSatisfy` isInfixOf "E-TYPE-WITHIN-UNPROVEN [static]: 'network' is not known here without running the program"
          out `shouldSatisfy` isInfixOf "#local reaches the whole host"
          out `shouldSatisfy` isInfixOf "the volume \"/var/run/docker.sock\" gives what the host has, which within { volumes: false } of the module does not allow"
          out `shouldSatisfy` isInfixOf "'release_of' is named as a value"
          -- The listings report what the constraints find, and do not fail.
          network <- runLask lask dir ["envs", "network"] ""
          resExit network `shouldBe` 0
          resOut network `shouldSatisfy` isInfixOf "exceeded  main.lask: bad → release_of"
          resOut network `shouldSatisfy` isInfixOf "ok        main.lask: ok → release_of"
          resOut network `shouldSatisfy` isInfixOf "unproven  main.lask: probe"
          access <- runLask lask dir ["envs", "access", "release-of"] ""
          resOut access `shouldSatisfy` isInfixOf "within { volumes: false } and within { outbound: false }"
    it "evaluates run options that can only compute, as a library builds them (spec 10.11)" $ \lask ->
      withProject
        [ ( "lib.lask",
            "export mount(dir: String | Null, target: String): Array<String> = case (dir) {\n\
            \  Null -> []\n\
            \  else -> [\"#{dir}:#{target}\"]\n\
            \}\n\
            \export setup(--region: String | Null = null, --config_dir: String | Null = null): Record<env: Map<String | Null>, volumes: Array<String>> = {\n\
            \  env: { \"REGION\": region },\n\
            \  volumes: mount(config_dir, \"/root/.cfg:ro\")\n\
            \}\n\
            \export env(--region: String | Null = null, --config_dir: String | Null = null): Runnable = do {\n\
            \  own = setup(region = region, config_dir = config_dir)\n\
            \  runnable(#alpine:3.20, volumes = concat_array(own.volumes, []))\n\
            \}\n"
          ),
          ( "main.lask",
            "within { volumes: false }\n\
            \import { env } from \"./lib.lask\"\n\
            \plain() = $[env(region = get_env(\"REGION\"))] true\n\
            \mounted() = $[env(config_dir = \"/home/me/.cfg\")] true\n\
            \unknown() = $[env(config_dir = get_env(\"CFG\"))] true\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["check"] ""
          let out = resOut r
          -- A field that only computes is known even where another
          -- field of the same record is read from the process.
          out `shouldNotSatisfy` isInfixOf "main.lask:3:"
          out `shouldSatisfy` isInfixOf "main.lask:4:13-4:54: E-TYPE-WITHIN-EXCEEDED [static]: \"/home/me/.cfg\" is mounted \"ro\""
          out `shouldSatisfy` isInfixOf "main.lask:5:13-5:53: E-TYPE-WITHIN-UNPROVEN"
    it "bounds the evaluation of a run option in steps, not in time (spec 10.11)" $ \lask ->
      withProject
        [ ( "main.lask",
            "within { outbound: false }\n\
            \spin(n: Number): String = spin(n + 1)\n\
            \none(n: Number): String = size(range(0, n)) < 0 ? \"bridge\" : \"none\"\n\
            \small() = $[runnable(#alpine:3.20, network = none(1000))] true\n\
            \large() = $[runnable(#alpine:3.20, network = none(2000000))] true\n\
            \endless() = $[runnable(#alpine:3.20, network = spin(0))] true\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["check"] ""
          let out = resOut r
          -- Within the bound the option is known, and keeps to the
          -- constraint; past it, or without end, it is not known.
          out `shouldNotSatisfy` isInfixOf "main.lask:4:"
          out `shouldSatisfy` isInfixOf "main.lask:5:"
          out `shouldSatisfy` isInfixOf "main.lask:6:"
          length (filter (isInfixOf "E-TYPE-WITHIN-UNPROVEN") (lines out)) `shouldBe` 2
    it "does not bound the evaluation of a run (spec 10.11)" $ \lask ->
      withProject [("main.lask", "none(n: Number): String = size(range(0, n)) < 0 ? \"bridge\" : \"none\"\n")] $ \dir -> do
        e <- runLask lask dir ["eval", "none", "2000000"] ""
        resOut e `shouldBe` "\"none\"\n"
    it "reports a broken constraint in what it covers, at the call that leads out of it (spec 10.11)" $ \lask ->
      withProject
        [ ( "main.lask",
            "c() within { files: \"ro\" } = do {\n\
            \  b()\n\
            \}\n\
            \b() within { files: \"rw\" } = do {\n\
            \  $[#alpine:3.20] touch x\n\
            \  $[#alpine:3.20] touch y\n\
            \}\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["check"] ""
          resExit r `shouldBe` 1
          -- b keeps to its own constraint; c's is broken by calling it.
          resOut r `shouldSatisfy` isInfixOf "main.lask:2:3-2:6: E-TYPE-WITHIN-EXCEEDED"
          resOut r `shouldNotSatisfy` isInfixOf "main.lask:5:"
          length (filter (isInfixOf "E-TYPE-WITHIN") (lines (resOut r))) `shouldBe` 1
    it "checks what an invocation gives a parameter before anything runs (spec 10.11)" $ \lask ->
      withProject
        [ ( "main.lask",
            "f(--net: String = \"none\"): String within { outbound: false } = $[runnable(#alpine:3.20, network = net)] echo hi\n"
          )
        ]
        $ \dir -> do
          static <- runLask lask dir ["check"] ""
          resExit static `shouldBe` 0
          widened <- runLask lask dir ["eval", "f", "--net", "bridge"] ""
          resExit widened `shouldBe` 1
          resErr widened `shouldSatisfy` isInfixOf "E-TYPE-WITHIN-EXCEEDED"
          resErr widened `shouldSatisfy` isInfixOf "the network is \"bridge\""
          kept <- runLask lask dir ["eval", "f"] ""
          resErr kept `shouldNotSatisfy` isInfixOf "E-TYPE-WITHIN"
    it "checks the command words cmd runs against the module constraint (spec 5)" $ \lask ->
      withProject [("main.lask", "within { outbound: false }\ncommand { \"uname\" } on #local\n")] $ \dir -> do
        -- Declaring a word on the host is not a use; running it is.
        static <- runLask lask dir ["check"] ""
        resExit static `shouldBe` 0
        r <- runLask lask dir ["cmd", "uname"] ""
        resExit r `shouldBe` 1
        resErr r `shouldSatisfy` isInfixOf "E-TYPE-WITHIN-EXCEEDED"
    it "follows secrets to where they are given, and bounds them with within (spec 10.11)" $ \lask ->
      withProject
        [ ( "main.lask",
            "aws(key: String) = #alpine:3.20{env: {\"AWS_SECRET_ACCESS_KEY\": key}}\n\
            \whoami(--key!!: String = get_env(\"K\")): String within { secrets: [key] } = $[aws(key)] true\n\
            \leak(--key!!: String = get_env(\"K\")): String within { secrets: false } = $[#alpine:3.20] echo #{key}\n\
            \token!!: String = get_env(\"TOKEN\")\n\
            \counted(): String within { secrets: false } = $[#alpine:3.20] echo #{length(token)} #{token == \"\"}\n\
            \cfg = {user: \"me\", pass: token}\n\
            \user_only(): String within { secrets: false } = $[#alpine:3.20] echo #{cfg.user}\n\
            \vars_of(t: String): Map<String> = merge({\"A\": t}, {\"B\": \"b\"})\n\
            \built(): String within { secrets: [token] } = $[#alpine:3.20{env: vars_of(token)}] true\n\
            \absent_of(--key!!: String | Null = null): Runnable = #alpine:3.20{env: {\"K\": key}}\n\
            \absent(): String within { secrets: false } = $[absent_of()] true\n\
            \named(): String within { secrets: false } = $[#alpine:3.20{hostname: token}] true\n\
            \shown(xs: Array<String>): Array<String> = for (x: xs) {\n\
            \  $[#alpine:3.20] echo #{x}\n\
            \}\n\
            \calls(): Array<String> = shown([token])\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["check"] ""
          resExit r `shouldBe` 1
          let out = resOut r
              withinLines = filter (isInfixOf "E-TYPE-WITHIN") (lines out)
          out `shouldSatisfy` isInfixOf "main.lask:3:74-3:101: E-TYPE-WITHIN-EXCEEDED [static]: the secret 'main.lask: leak --key' is given in the command string"
          out `shouldSatisfy` isInfixOf "main.lask:12:45-12:82: E-TYPE-WITHIN-EXCEEDED [static]: the secret 'main.lask: token' is given in the run option 'hostname'"
          -- A secret in the variable the constraint names, one that is
          -- only counted or compared, a field beside it, a key kept
          -- through merge, and an absent one are all within.
          length withinLines `shouldBe` 2
          listing <- runLask lask dir ["envs", "secrets"] ""
          resExit listing `shouldBe` 0
          let rows = lines (resOut listing)
              row secret channel = filter (\l -> secret `isInfixOf` l && channel `isInfixOf` l) rows
          row "main.lask: whoami --key" "env AWS_SECRET_ACCESS_KEY" `shouldSatisfy` (\ls -> length ls == 1 && all (isInfixOf "ok") ls)
          row "main.lask: leak --key" "command !" `shouldSatisfy` (\ls -> length ls == 1 && all (isInfixOf "exceeded") ls)
          row "main.lask: token" "env A " `shouldSatisfy` ((== 1) . length)
          row "main.lask: token" "command !" `shouldSatisfy` (\ls -> length ls == 1 && all (isInfixOf "main.lask: calls → shown") ls)
          filter (isInfixOf "absent") rows `shouldBe` []
          -- A parameter the command line may give is a secret there.
          length (filter (isInfixOf "main.lask: absent_of --key") rows) `shouldBe` 0
          json <- runLask lask dir ["envs", "secrets", "--output", "json"] ""
          resOut json `shouldSatisfy` isInfixOf "\"channel\":{\"kind\":\"env\",\"name\":\"AWS_SECRET_ACCESS_KEY\"}"
          scoped <- runLask lask dir ["envs", "secrets", "whoami"] ""
          resOut scoped `shouldSatisfy` isInfixOf "within { secrets: [key] }"
    it "advises a secret on a command line, and nothing for one given in env (spec 10.11, 14.2)" $ \lask ->
      withProject
        [ ( "main.lask",
            "token!!: String = get_env(\"TOKEN\")\n\
            \inline(): String = $[#alpine:3.20] echo #{token}\n\
            \given(): String = $[#alpine:3.20{env: {\"TOKEN\": token}}] echo \"$TOKEN\"\n\
            \counted(): String = $[#alpine:3.20] echo #{length(token)}\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["check"] ""
          resExit r `shouldBe` 0
          let advised = filter (isInfixOf "W-SECRET-COMMAND-LINE") (lines (resOut r))
          advised `shouldBe` ["main.lask:2:20-2:49: W-SECRET-COMMAND-LINE [static]: the secret 'main.lask: token' is in the command string, from which the programs the command runs are given it as arguments, which the host's process list shows; give it in 'env' to a program that reads the variable itself"]
    it "admits a secret by a name it is bound to with !! (spec 10.11)" $ \lask ->
      withProject
        [ ( "main.lask",
            "token!!: String = get_env(\"TOKEN\")\n\
            \show(v!!: String): String within { secrets: [v] } = $[#alpine:3.20{env: {\"V\": v}}] true\n\
            \ok(): String = show(token)\n\
            \plain(v: String): String within { secrets: false } = $[#alpine:3.20{env: {\"V\": v}}] true\n\
            \via_plain(): String = plain(token)\n\
            \issue(): String within { secrets: [generated] } = do {\n\
            \  generated!! = random_string(12)\n\
            \  $[#alpine:3.20{env: {\"G\": generated}}] true\n\
            \}\n\
            \anon(): String within { secrets: [token] } = $[#alpine:3.20{env: {\"G\": mark_secret(get_env(\"Z\"))}}] true\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["check"] ""
          resExit r `shouldBe` 1
          let withinLines = filter (isInfixOf "E-TYPE-WITHIN") (lines (resOut r))
          -- show names what it is given by its own parameter, and issue
          -- what it makes by its binding; an unmarked parameter names
          -- nothing, and nor does an unbound mark_secret.
          length withinLines `shouldBe` 2
          resOut r `shouldSatisfy` isInfixOf "main.lask:5:23-5:35: E-TYPE-WITHIN-EXCEEDED [static]: the secret 'main.lask: token' is given in the variable V of 'env', which within { secrets: false } of 'plain'"
          resOut r `shouldSatisfy` isInfixOf "bound to no name a constraint can give (bind it with !!)"
    it "rejects a secrets field that is not one (spec 10.11)" $ \lask ->
      do
        withProject [("main.lask", "token!!: String = get_env(\"T\")\nf() within { secrets: [tokn] } = $[#alpine:3.20] true\n")] $ \dir -> do
          r <- runLask lask dir ["check"] ""
          resExit r `shouldBe` 1
          resOut r `shouldSatisfy` isInfixOf "main.lask:2:24-2:28: E-TYPE-WITHIN-SPEC [static]: 'tokn' is not a secret binding of 'f'"
        -- A name is an identifier, not the name of a variable.
        withProject [("main.lask", "g() within { secrets: [\"TOKEN\"] } = $[#alpine:3.20] true\n")] $ \dir -> do
          r <- runLask lask dir ["check"] ""
          resExit r `shouldBe` 1
          resOut r `shouldSatisfy` isInfixOf "'secrets' takes false, or an array of the names of secret bindings, such as [token]"
    it "checks a list written against a union with one array as that array (spec 4.4)" $ \lask ->
      withProject [("main.lask", "takes(--hosts: Array<String> | Null = null): String = \"x\"\ne(): String = takes(hosts = [])\nr = #alpine:3.20{egress: []}\n")] $ \dir -> do
        r <- runLask lask dir ["check"] ""
        resExit r `shouldBe` 0
    it "names constraints with access, and bounds runs and inbound (spec 5, 10.11)" $ \lask -> do
      withProject
        [ ("policy.lask", "export access Offline = { runs: [\"container\"], outbound: false, inbound: false, volumes: false }\n"),
          ( "main.lask",
            "import { Offline } from \"./policy.lask\"\n\
            \import * as p from \"./policy.lask\"\n\
            \access Pure = { runs: false }\n\
            \access Loop = { inbound: [\"127.0.0.1:8080\", \"::1\"] }\n\
            \a() within Offline = $[#alpine:3.22{network: \"none\"}] true\n\
            \b() within p.Offline = $[#alpine:3.22] true\n\
            \c() within Pure = $[#alpine:3.22] true\n\
            \d(e: Runnable) within Pure = $[e] true\n\
            \f() within Pure = \"pure\"\n\
            \g() within Loop = $[#nginx:1.27{publish: [\"127.0.0.1:8080:80\", \"[::1]:9000:80\"]}] true\n\
            \h() within Loop = $[#nginx:1.27{publish: [\"8080:80\"]}] true\n\
            \i() within { runs: [\"container\"] } = $[#local] true\n\
            \j() within { runs: [\"local\"] } = $[#local] true\n\
            \k() within { outbound: true, volumes: true } = $[#alpine:3.22{volumes: [\"/data:/d\"]}] true\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["check"] ""
          resExit r `shouldBe` 1
          let withinLines = filter (isInfixOf "E-TYPE-WITHIN") (lines (resOut r))
          length withinLines `shouldBe` 5
          resOut r `shouldSatisfy` isInfixOf "main.lask:6:24-6:44: E-TYPE-WITHIN-EXCEEDED [static]: the network is \"default\", which within p.Offline of 'b' does not allow (outbound)"
          resOut r `shouldSatisfy` isInfixOf "it runs in a container, which within Pure of 'c' does not allow (runs)"
          resOut r `shouldSatisfy` isInfixOf "a command runs here, which within Pure of 'd' does not allow (runs)"
          resOut r `shouldSatisfy` isInfixOf "a port is published on 0.0.0.0:8080, which within Loop of 'h' does not allow (inbound)"
          resOut r `shouldSatisfy` isInfixOf "#local reaches the whole host, which within { runs: [\"container\"] } of 'i' does not allow (runs)"
      withProject
        [ ( "main.lask",
            "type T = String\n\
            \access Pure = { runs: false }\n\
            \access Named = { secrets: [x] }\n\
            \k() within T = \"x\"\n\
            \l(x: Pure) = x\n\
            \m() within Pur = 1\n\
            \n() within { network: \"none\" } = 1\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["check"] ""
          resExit r `shouldBe` 1
          resOut r `shouldSatisfy` isInfixOf "main.lask:3:28-3:29: E-TYPE-WITHIN-SPEC [static]: an access declaration gives 'secrets' only as false"
          resOut r `shouldSatisfy` isInfixOf "main.lask:4:12-4:13: E-TYPE-WITHIN-SPEC [static]: 'T' is a type, not an access constraint"
          resOut r `shouldSatisfy` isInfixOf "main.lask:5:6-5:10: E-NAME-UNDEFINED [static]: 'Pure' is an access constraint, not a type"
          resOut r `shouldSatisfy` isInfixOf "main.lask:6:12-6:15: E-NAME-UNDEFINED [static]: undefined access constraint: 'Pur'"
          resOut r `shouldSatisfy` isInfixOf "unknown field 'network': a constraint takes files, volumes, outbound, inbound, secrets and runs"
    it "combines access constraints with &, each conjunct checked as written (spec 5, 10.11)" $ \lask -> do
      withProject
        [ ("policy.lask", "export access Contained = { runs: [\"container\"] }\n"),
          ( "main.lask",
            "import * as p from \"./policy.lask\"\n\
            \access Offline = { outbound: false, inbound: false }\n\
            \access Sealed = Offline &\n\
            \  p.Contained\n\
            \quiet = #alpine:3.22{ base_access: \"ro\", network: \"none\" }\n\
            \a(): String within Sealed & { files: \"ro\" } = $[quiet] true\n\
            \b(): String within Sealed = $[#alpine:3.22] true\n\
            \c(): String within p.Contained & { files: \"ro\" } = $[#local] true\n\
            \d(): String within { outbound: [\"*.github.com:443\"] } & { outbound: [\"api.github.com:443\", \"x.com:443\"] } = $[#alpine:3.22{egress: [\"api.github.com:443\"]}] true\n\
            \e(): String within { outbound: [\"*.github.com:443\"] } & { outbound: [\"x.com:443\"] } = $[#alpine:3.22{egress: [\"api.github.com:443\"]}] true\n\
            \f(): String within { files: \"ro\" } & { files: \"rw\" } = $[quiet] true\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["check"] ""
          resExit r `shouldBe` 1
          let withinLines = filter (isInfixOf "E-TYPE-WITHIN") (lines (resOut r))
          length withinLines `shouldBe` 4
          resOut r `shouldSatisfy` isInfixOf "the network is \"default\", which within Sealed of 'b' does not allow (outbound)"
          resOut r `shouldSatisfy` isInfixOf "#local reaches the whole host, which within p.Contained of 'c' does not allow (runs)"
          resOut r `shouldSatisfy` isInfixOf "#local reaches the whole host, which within { files: \"ro\" } of 'c' does not allow (files)"
          resOut r `shouldSatisfy` isInfixOf "within { outbound: [\"x.com:443\"] } of 'e' does not allow (outbound)"
          a <- runLask lask dir ["envs", "access", "a"] ""
          take 1 (lines (resOut a)) `shouldBe` ["within Sealed & { files: \"ro\" }"]
      withProject
        [ ( "main.lask",
            "type T = String\n\
            \access A = B & { files: \"ro\" }\n\
            \access B = A\n\
            \access C = B & T\n\
            \access D = { runs: false, runs: false } & { runs: false }\n\
            \f(): String within C = \"x\"\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["check"] ""
          resExit r `shouldBe` 1
          resOut r `shouldSatisfy` isInfixOf "main.lask:2:12-2:13: E-TYPE-WITHIN-SPEC [static]: 'A' is written with 'B', which leads back to it"
          resOut r `shouldSatisfy` isInfixOf "main.lask:3:12-3:13: E-TYPE-WITHIN-SPEC [static]: 'B' is written with 'A', which leads back to it"
          resOut r `shouldSatisfy` isInfixOf "main.lask:4:16-4:17: E-TYPE-WITHIN-SPEC [static]: 'T' is a type, not an access constraint"
          resOut r `shouldSatisfy` isInfixOf "main.lask:5:27-5:31: E-TYPE-WITHIN-SPEC [static]: duplicate field 'runs'"
          length (filter (isInfixOf "E-TYPE-WITHIN") (lines (resOut r))) `shouldBe` 4
    it "checks egress entries, and what within admits of them (spec 10.11, 10.12)" $ \lask -> do
      withProject [("main.lask", "a = #alpine:3.20{egress: [\"x.com\"]}\n")] $ \dir -> do
        r <- runLask lask dir ["check"] ""
        resOut r `shouldSatisfy` isInfixOf "main.lask:1:27-1:34: E-TYPE-ENV-CONSTRUCT [static]: 'x.com' is not <host>:<port>"
      withProject [("main.lask", "a = #alpine:3.20{egress: [\"x.com:443\"], network: \"none\"}\n")] $ \dir -> do
        r <- runLask lask dir ["check"] ""
        resOut r `shouldSatisfy` isInfixOf "E-TYPE-ENV-CONSTRUCT [static]: 'network' is given with egress"
      withProject
        [ ( "main.lask",
            "npm = #alpine:3.20{egress: [\"registry.npmjs.org:443\"]}\n\
            \a(): String within { outbound: [\"registry.npmjs.org:443\"] } = $[npm] true\n\
            \b(): String within { outbound: [\"*.github.com:443\"] } = $[#alpine:3.20{egress: [\"api.github.com:443\", \"github.com:443\"]}] true\n\
            \c(): String within { outbound: false } = $[#alpine:3.20{egress: []}] true\n\
            \d(): String within { outbound: [\"a.com:443\"] } = $[#alpine:3.20] true\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["check"] ""
          resExit r `shouldBe` 1
          let withinLines = filter (isInfixOf "E-TYPE-WITHIN") (lines (resOut r))
          -- A wildcard does not cover the name itself, an egress that
          -- lists nothing is within none, and a container that may
          -- reach anything is within no list.
          length withinLines `shouldBe` 2
          resOut r `shouldSatisfy` isInfixOf "E-TYPE-WITHIN-EXCEEDED [static]: egress lists \"github.com:443\", which within { outbound: [\"*.github.com:443\"] } of 'b'"
          resOut r `shouldSatisfy` isInfixOf "E-TYPE-WITHIN-EXCEEDED [static]: the network is \"default\", which within { outbound: [\"a.com:443\"] } of 'd'"
          network <- runLask lask dir ["envs", "network"] ""
          resOut network `shouldSatisfy` isInfixOf "registry.npmjs.org:443"
          resOut network `shouldSatisfy` isInfixOf "(none)"
          json <- runLask lask dir ["envs", "network", "--output", "json"] ""
          resOut json `shouldSatisfy` isInfixOf "\"class\":\"egress\",\"egress\":[\"registry.npmjs.org:443\"]"
    it "places a container given egress beside its proxy, logs what it refuses, and removes both (spec 10.12)" $ \lask ->
      withFakeDocker $ \state extra ->
        withProject [("main.lask", "f(): String = $[#alpine:3.22.2{egress: [\"example.com:443\"]}] echo hi\n")] $ \dir -> do
          _ <- runLaskEnv lask dir extra ["sync"] ""
          writeFile (state </> "present" </> "lask-egress-proxy_dev") "[]"
          writeFile (state </> "denied") "{\"event\":\"denied\",\"destination\":\"other.test:443\",\"reason\":\"not in egress\"}\n"
          writeFile (state </> "calls") ""
          r <- runLaskEnv lask dir (("LASK_EGRESS_PROXY_IMAGE", "lask-egress-proxy:dev") : extra) ["eval", "f"] ""
          resExit r `shouldBe` 0
          resErr r `shouldSatisfy` isInfixOf "2| egress denied: other.test:443"
          cs <- calls state
          let has p = filter (isInfixOf p) cs
          has "network create --internal" `shouldSatisfy` ((== 1) . length)
          -- The proxy is on both networks at once, and known on the
          -- inside one by the name every container gives egress uses.
          has ("--network name=") `shouldSatisfy` all (isInfixOf "alias=lask-egress-proxy")
          has "lask-egress-proxy:dev -listen :3128 example.com:443" `shouldSatisfy` ((== 1) . length)
          -- The container is on the inside network only, and told
          -- where the proxy is.
          let workload = [c | c <- cs, "run " `isPrefixOf` c, " --rm " `isInfixOf` c]
          workload `shouldSatisfy` ((== 1) . length)
          -- The proxy's variables are ones the client reads itself, so
          -- they are given in a file (spec 10.2).
          workload `shouldSatisfy` all (isInfixOf "--env-file ")
          given <- lines <$> readFile (state </> "env")
          given `shouldSatisfy` elem "HTTPS_PROXY=http://lask-egress-proxy:3128"
          given `shouldSatisfy` elem "NODE_USE_ENV_PROXY=1"
          workload `shouldSatisfy` all (isInfixOf "-in ")
          workload `shouldSatisfy` all (isInfixOf "--network lask-egress-")
          -- Both are removed when the invocation ends.
          has "rm --force lask-egress-" `shouldSatisfy` ((== 1) . length)
          has "network rm lask-egress-" `shouldSatisfy` ((== 2) . length)
    it "runs a container given egress with the proxy sync materialized, and not without one (spec 10.12)" $ \lask ->
      withFakeDocker $ \_ extra ->
        withProject [("main.lask", "f(): String = $[#alpine:3.22.2{egress: [\"example.com:443\"]}] echo hi\n")] $ \dir -> do
          _ <- runLaskEnv lask dir extra ["sync"] ""
          missing <- runLaskEnv lask dir (("LASK_EGRESS_PROXY_IMAGE", "lask-egress-proxy:absent") : extra) ["eval", "f"] ""
          resExit missing `shouldBe` 3
          resErr missing `shouldSatisfy` isInfixOf "E-IO-IMAGE-MISSING"
          -- The proxy this release names is one sync materialized,
          -- pinned by its digest.
          named <- runLaskEnv lask dir extra ["eval", "f"] ""
          resExit named `shouldBe` 0
          lock <- readFile (dir </> "lask.lock.json")
          lock `shouldSatisfy` isInfixOf "#ghcr.io/lask-task-runner/lask-egress-proxy@sha256:"
    it "gives a variable of env the client reads itself in a file, on no command line (spec 10.2)" $ \lask ->
      withFakeDocker $ \state extra ->
        withProject [("main.lask", "token!!: String = get_env_or(\"T\", \"s3cr3t-value\")\nf(): String within { secrets: [token] } = $[runnable(#alpine:3.22.2, env = {\"DOCKER_TOKEN\": token, \"HTTPS_PROXY\": \"http://p:3128\"})] true\nbad(): String = $[runnable(#alpine:3.22.2, env = {\"DOCKER_X\": \"a\\nb\"})] true\n")] $ \dir -> do
          _ <- runLaskEnv lask dir extra ["sync"] ""
          writeFile (state </> "calls") ""
          r <- runLaskEnv lask dir extra ["eval", "f"] ""
          resExit r `shouldBe` 0
          cs <- calls state
          let runs = [c | c <- cs, "run " `isPrefixOf` c, " --rm " `isInfixOf` c]
          runs `shouldSatisfy` ((== 1) . length)
          runs `shouldSatisfy` all (isInfixOf "--env-file ")
          runs `shouldSatisfy` all (not . isInfixOf "s3cr3t-value")
          runs `shouldSatisfy` all (not . isInfixOf "http://p:3128")
          given <- lines <$> readFile (state </> "env")
          given `shouldSatisfy` elem "DOCKER_TOKEN=s3cr3t-value"
          given `shouldSatisfy` elem "HTTPS_PROXY=http://p:3128"
          -- The file is removed once the client has exited.
          let file = takeWhile (/= ' ') (snd (breakAfter "--env-file " (concat (take 1 runs))))
              breakAfter needle hay = case [i | i <- [0 .. length hay - length needle], take (length needle) (drop i hay) == needle] of
                i : _ -> (take i hay, drop (i + length needle) hay)
                [] -> (hay, "")
          doesFileExist file `shouldReturn` False
          -- A value a line of the file cannot hold is refused, not put
          -- on the command line.
          b <- runLaskEnv lask dir extra ["eval", "bad"] ""
          resExit b `shouldBe` 3
          resErr b `shouldSatisfy` isInfixOf "cannot hold"
    it "gives a container's command to its shell on stdin, on no command line (spec 10.2)" $ \lask ->
      withFakeDocker $ \state extra ->
        withProject [("main.lask", "token!!: String = get_env_or(\"T\", \"s3cr3t-value\")\nf(): String = $[#alpine:3.22.2] echo marker-#{token}\n")] $ \dir -> do
          _ <- runLaskEnv lask dir extra ["sync"] ""
          writeFile (state </> "calls") ""
          r <- runLaskEnv lask dir extra ["eval", "f"] ""
          resExit r `shouldBe` 0
          cs <- calls state
          let runs = [c | c <- cs, "run " `isPrefixOf` c, " --rm " `isInfixOf` c]
          runs `shouldSatisfy` ((== 1) . length)
          -- The client is given a shell that reads its command, not the
          -- command: the secret is on no command line.
          runs `shouldSatisfy` all (isInfixOf " -i ")
          runs `shouldSatisfy` all (isInfixOf "lask_command=")
          runs `shouldSatisfy` all (not . isInfixOf "s3cr3t-value")
          runs `shouldSatisfy` all (not . isInfixOf "marker-")
    it "takes a volume that gives what the host has as the host (spec 10.11)" $ \lask ->
      withProject
        [ ( "main.lask",
            "within { volumes: { \"/var/run\": \"rw\", \"/home/me\": \"ro\" } }\n\
            \ship(): String = $[#docker:27{volumes: [\"/var/run/docker.sock:/var/run/docker.sock\"]}] docker ps\n\
            \up(): String = $[#alpine:3.20{volumes: [\"/var/run/../lib/docker:/d:ro\"]}] ls\n\
            \cfg(): String = $[#alpine:3.20{volumes: [\"/home/me/.cfg:/cfg:ro\"]}] ls /cfg\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["check"] ""
          resExit r `shouldBe` 1
          let withinLines = filter (isInfixOf "E-TYPE-WITHIN") (lines (resOut r))
          -- A constraint that admits the socket's directory does not
          -- make the socket less than the host; a path is read with ..
          -- resolved; an ordinary directory is what it says.
          length withinLines `shouldBe` 2
          resOut r `shouldSatisfy` isInfixOf "the volume \"/var/run/docker.sock\" gives what the host has"
          resOut r `shouldSatisfy` isInfixOf "the volume \"/var/lib/docker\" gives what the host has"
          files <- runLask lask dir ["envs", "files"] ""
          resOut files `shouldSatisfy` isInfixOf "rw ! (host)"
          resOut files `shouldSatisfy` isInfixOf "ro !\n"
    it "builds a recipe given build_network = \"none\" with no network, as an image of its own (spec 10.3)" $ \lask ->
      withFakeDocker $ \state extra ->
        withProject
          [ ("img/Dockerfile", "FROM alpine:3.22.2\n"),
            ( "main.lask",
              "online = #./img/Dockerfile\n\
              \offline = #./img/Dockerfile(build_network = \"none\")\n\
              \a(): String = $[online] true\n\
              \b(): String = $[offline] true\n"
            )
          ]
          $ \dir -> do
            s' <- runLaskEnv lask dir extra ["sync"] ""
            resExit s' `shouldBe` 0
            resOut s' `shouldSatisfy` isInfixOf "#./img/Dockerfile (no network)"
            cs <- calls state
            let builds = [c | c <- cs, "build " `isPrefixOf` c]
                tag c = takeWhile (/= ' ') (drop 4 (snd (breakOn' " -t " c)))
                breakOn' needle hay = case [i | i <- [0 .. length hay - length needle], take (length needle) (drop i hay) == needle] of
                  i : _ -> (take i hay, drop i hay)
                  [] -> (hay, "")
            length builds `shouldBe` 2
            length (filter (isInfixOf "--network none") builds) `shouldBe` 1
            -- Two images, so that the offline one is never the other.
            length (nub (map tag builds)) `shouldBe` 2
            lock <- readFile (dir </> "lask.lock.json")
            lock `shouldSatisfy` isInfixOf "#img/Dockerfile(build_network = \\\"none\\\")"
            bad <- withProject [("main.lask", "x = #./Dockerfile(build_network = \"host\")\n"), ("Dockerfile", "FROM alpine\n")] $ \d -> runLask lask d ["check"] ""
            resOut bad `shouldSatisfy` isInfixOf "'build_network' takes \"none\" or \"default\", not 'host'"
    it "rejects undefined environment names before evaluation" $ \lask ->
      withProject [("main.lask", "f() = $[#env(\"missing\")] ls\n")] $ \dir -> do
        r <- runLask lask dir ["run", "f"] ""
        resExit r `shouldBe` 1
        resErr r `shouldSatisfy` isInfixOf "E-TYPE-ENV-CONSTRUCT"
