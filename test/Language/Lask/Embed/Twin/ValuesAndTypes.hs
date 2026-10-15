{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The EDSL twin of example/02-language/01-values-and-types, for the
-- declarations the first phase covers (#91). Each has its Lask source
-- above it; EmbedTwinSpec compares the two Cores.
module Language.Lask.Embed.Twin.ValuesAndTypes
  ( Config,
    app,
    port,
    staging,
    production,
    limits,
    limitOf,
    asNumber,
    firstOr,
    headNumber,
    headName,
    program,
  )
where

import Data.Text (Text)
import Language.Lask.Embed

-- app = "orders"
app :: ValueDecl 'TString
app = value "app" "orders"

-- port: Number = 8080
port :: ValueDecl 'TNumber
port = value "port" 8080

-- type Port = Number
type Port = 'TNumber

-- type Config = Record<name: String, port: Port, replicas: Number | Null, tags?: Array<String>>
type Config =
  Rec
    '[ "name" := 'TString,
       "port" := Port,
       "replicas" := 'TNumber :| 'TNull,
       "tags" :? 'TArray 'TString
     ]

-- staging: Config = { name: app, port: port, replicas: null }
staging :: ValueDecl Config
staging =
  value "staging" $
    recordAs @Config (fld @"name" (ref app) :& fld @"port" (ref port) :& fld @"replicas" null_ :& RNil)

-- production: Config = { name: app, port: 443, replicas: 3, tags: ["prod", "eu-west-1"] }
production :: ValueDecl Config
production =
  value "production" $
    recordAs @Config
      ( fld @"name" (ref app)
          :& fld @"port" 443
          :& fld @"replicas" 3
          :& fld @"tags" (list ["prod", "eu-west-1"])
          :& RNil
      )

-- limits: Map<Number> = { "cpu": 2, "memory": 4096 }
limits :: ValueDecl ('TMap 'TNumber)
limits = value "limits" (mapOf [("cpu", 2), ("memory", 4096)])

-- limit_of(name: String): Number = get_or(limits, name, 0)
limitOf :: Task '["name" ::: 'TString] 'TNumber
limitOf = task "limit_of" $ \name -> getOr (ref limits) name 0

-- as_number(v: Any): Number = cast(v)
asNumber :: Task '["v" ::: 'TAny] 'TNumber
asNumber = task "as_number" $ \v -> cast @'TNumber v

-- first_or<T>(xs: Array<T>, fallback: T): T =
--   if (is_empty(xs)) { fallback } else { xs[0] }
type T = 'TVar "T" 'BNone

firstOr :: Generic '[ '("T", 'BNone)] '["xs" ::: 'TArray T, "fallback" ::: T] T
firstOr = generic "first_or" $ \xs fallback -> if_ (isEmpty xs) fallback (at xs 0)

-- head_number(): Number = first_or([], 0)
headNumber :: Task '[] 'TNumber
headNumber = task "head_number" $ call (inst firstOr) (list []) 0

-- head_name(): String = first_or(["lask"], "none")
headName :: Task '[] 'TString
headName = task "head_name" $ call (inst firstOr) (list ["lask"]) "none"

program :: Either [Text] Program
program =
  assemble
    [ export staging,
      export production,
      export limitOf,
      export asNumber,
      export headNumber,
      export headName
    ]
