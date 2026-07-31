# Lask Language Specification

This document is the language specification of Lask that satisfies the requirements definition document (`requirements.md`).
This specification is intended to serve as the reference for implementation, verification, and editor integration.


## Table of Contents

- [1. Introduction](#1-introduction)
  - [1.1 Purpose](#11-purpose)
  - [1.2 Intended Audience](#12-intended-audience)
  - [1.3 Scope of the Specification](#13-scope-of-the-specification)
  - [1.4 Out of Scope](#14-out-of-scope)
  - [1.5 Relationship to the Requirements Definition Document](#15-relationship-to-the-requirements-definition-document)
  - [1.6 Terminology](#16-terminology)
- [2. Notation](#2-notation)
- [3. Lexical Specification](#3-lexical-specification)
  - [3.1 Comments](#31-comments)
  - [3.2 Identifiers](#32-identifiers)
  - [3.3 Literals and Environment Tokens](#33-literals-and-environment-tokens)
- [4. Type System](#4-type-system)
  - [4.1 Classification of Types](#41-classification-of-types)
  - [4.2 Type Syntax](#42-type-syntax)
  - [4.3 Type Annotations and Inference](#43-type-annotations-and-inference)
  - [4.4 Type Semantics](#44-type-semantics)
  - [4.5 Serializable and Non-serializable Types](#45-serializable-and-non-serializable-types)
- [5. Declarations and Modules](#5-declarations-and-modules)
- [6. Expressions](#6-expressions)
  - [6.1 Function Parameters, Lambda Expressions, and Higher-Order Functions](#61-function-parameters-lambda-expressions-and-higher-order-functions)
  - [6.2 Operators](#62-operators)
  - [6.3 Asynchronous Invocation and Awaiting](#63-asynchronous-invocation-and-awaiting)
  - [6.4 Control Structures](#64-control-structures)
  - [6.5 Procedural Notation](#65-procedural-notation)
  - [6.6 Command Execution Expressions](#66-command-execution-expressions)
  - [6.7 Environment Expressions](#67-environment-expressions)
  - [6.8 Accessor Expressions](#68-accessor-expressions)
  - [6.9 Error Handling Expressions](#69-error-handling-expressions)
- [7. Static Semantics](#7-static-semantics)
  - [7.1 Verification Context](#71-verification-context)
  - [7.2 Name Resolution Order](#72-name-resolution-order)
  - [7.3 Scope and Shadowing](#73-scope-and-shadowing)
  - [7.4 Integration of Type Annotations and Inference](#74-integration-of-type-annotations-and-inference)
  - [7.5 Call Consistency](#75-call-consistency)
  - [7.6 Static Expansion Order of Syntactic Sugar](#76-static-expansion-order-of-syntactic-sugar)
  - [7.7 Static Errors](#77-static-errors)
- [8. Dynamic Semantics](#8-dynamic-semantics)
  - [8.1 Evaluation Relation](#81-evaluation-relation)
  - [8.2 Values and Closures](#82-values-and-closures)
  - [8.3 Function Application](#83-function-application)
  - [8.4 Sequential Execution of `do`](#84-sequential-execution-of-do)
  - [8.5 Control Structures (Core Functions)](#85-control-structures-core-functions)
  - [8.6 Asynchrony (Core Functions)](#86-asynchrony-core-functions)
  - [8.7 Command Execution (Core Function)](#87-command-execution-core-function)
  - [8.8 Evaluation of Environment Expressions (Core Expression)](#88-evaluation-of-environment-expressions-core-expression)
  - [8.9 Evaluation of Accessor Expressions (Core Expression)](#89-evaluation-of-accessor-expressions-core-expression)
  - [8.10 Failure Propagation and Recovery](#810-failure-propagation-and-recovery)
- [9. Standard I/O and Data Flow](#9-standard-io-and-data-flow)
  - [9.1 I/O Channel Model](#91-io-channel-model)
  - [9.2 Ingestion of Standard Input](#92-ingestion-of-standard-input)
  - [9.3 Standard Input Reference Variable](#93-standard-input-reference-variable)
  - [9.4 Standard Input Decoding](#94-standard-input-decoding)
  - [9.5 Role of Standard Output](#95-role-of-standard-output)
  - [9.6 Role of Standard Error](#96-role-of-standard-error)
  - [9.7 Inter-Function Data Flow and Pipes](#97-inter-function-data-flow-and-pipes)
- [10. Execution Environments](#10-execution-environments)
  - [10.1 The `Environment` Type and Environment Expressions](#101-the-environment-type-and-environment-expressions)
  - [10.2 Target Environment Profiles and Environment Constructor Signatures](#102-target-environment-profiles-and-environment-constructor-signatures)
  - [10.3 Environment Definition File](#103-environment-definition-file)
  - [10.4 Environment Resolution Rules](#104-environment-resolution-rules)
  - [10.5 Working Directory Rules](#105-working-directory-rules)
  - [10.6 Environment Variable Rules](#106-environment-variable-rules)
  - [10.7 Permission Boundary](#107-permission-boundary)
  - [10.8 Responsibilities for Absorbing Environment Differences](#108-responsibilities-for-absorbing-environment-differences)
  - [10.9 The SSH Execution Model for `remote`](#109-the-ssh-execution-model-for-remote)
- [11. CLI Specification](#11-cli-specification)
  - [11.1 Subcommands](#111-subcommands)
  - [11.2 Function Invocation](#112-function-invocation)
  - [11.3 Input/Output Contract](#113-inputoutput-contract)
  - [11.4 Environment Check (`envs`)](#114-environment-check-envs)
  - [11.5 Dependency Management (`deps`)](#115-dependency-management-deps)
- [12. Observability](#12-observability)
  - [12.1 Observation Targets and Design Principles](#121-observation-targets-and-design-principles)
  - [12.2 Execution Log](#122-execution-log)
  - [12.3 Command Execution Log](#123-command-execution-log)
  - [12.4 Stack Traces](#124-stack-traces)
  - [12.5 Trace Identifier](#125-trace-identifier)
  - [12.6 Execution Event Output](#126-execution-event-output)
  - [12.7 In-flight Diagnostics](#127-in-flight-diagnostics)
  - [12.8 Protection of Sensitive Information and Retention Policy](#128-protection-of-sensitive-information-and-retention-policy)
- [13. Serialization Conventions](#13-serialization-conventions)
  - [13.1 Data Values](#131-data-values)
  - [13.2 Function Values](#132-function-values)
  - [13.3 Execution Events](#133-execution-events)
- [14. Error System](#14-error-system)
  - [14.1 Error Classification](#141-error-classification)
  - [14.2 Error Code Conventions](#142-error-code-conventions)
  - [14.3 Minimum Requirements for Diagnostic Information](#143-minimum-requirements-for-diagnostic-information)
  - [14.4 Static Errors](#144-static-errors)
  - [14.5 Runtime Errors](#145-runtime-errors)
  - [14.6 External I/O Errors](#146-external-io-errors)
  - [14.7 Recoverability and Propagation Rules](#147-recoverability-and-propagation-rules)
  - [14.8 Correspondence to CLI Exit Codes](#148-correspondence-to-cli-exit-codes)
- [15. Built-in Library](#15-built-in-library)
  - [15.1 Provision Policy](#151-provision-policy)
  - [15.2 Numeric Operations](#152-numeric-operations)
  - [15.3 String Operations](#153-string-operations)
  - [15.4 Array, Map, and Record Operations](#154-array-map-and-record-operations)
  - [15.5 Command Execution Functions](#155-command-execution-functions)
  - [15.6 Parallel and Asynchronous Helper Functions](#156-parallel-and-asynchronous-helper-functions)
  - [15.7 Error Handling Functions](#157-error-handling-functions)
  - [15.8 Serialization and Type-Migration Helper Functions](#158-serialization-and-type-migration-helper-functions)
  - [15.9 Error Contract](#159-error-contract)
- [16. Examples](#16-examples)
  - [16.1 Minimal Program](#161-minimal-program)
  - [16.2 Functions with Type Annotations](#162-functions-with-type-annotations)
  - [16.3 Higher-Order Functions and Composition](#163-higher-order-functions-and-composition)
  - [16.4 Arrays, Maps, and Records](#164-arrays-maps-and-records)
  - [16.5 Procedural Notation and Command Execution with Environments](#165-procedural-notation-and-command-execution-with-environments)
  - [16.6 Asynchronous Execution](#166-asynchronous-execution)
  - [16.7 CLI Execution Examples](#167-cli-execution-examples)
  - [16.8 Execution Event Example](#168-execution-event-example)
  - [16.9 Error Handling and Exit Codes](#169-error-handling-and-exit-codes)

## 1. Introduction

### 1.1 Purpose

Lask aims to express tasks as functions and to describe automation and CI/CD execution in a single consistent language.
This specification defines syntax, types, evaluation, input/output, and observability so that implementers, users, and tool developers can share the same semantics.

The design goals of Lask are as follows.

- Declarative expressiveness centered on function composition
- Readability that allows users accustomed to procedural styles to adopt the language incrementally
- Safety through pre-execution verification (syntax, references, types)
- Reproducibility by absorbing differences between execution environments
- High observability through execution logs and traces

### 1.2 Intended Audience

The intended audience of this specification is as follows.

- Developers implementing language processors (parsers, type checkers, executors)
- Developers implementing peripheral tools such as CLI, LSP, REPL, and log collection
- Users and operators designing operational rules that conform to the language specification

### 1.3 Scope of the Specification

This specification covers the following.

- Lexical specification, syntactic specification, and type system
- Declarations, expressions, modules, and control structures
- Static semantics and dynamic semantics
- CLI specification, standard input/output, and execution environments
- Serialization conventions, the error system, and observability (logs/traces/events)

### 1.4 Out of Scope

The following are outside the scope of this specification.

- Optimization techniques that depend on specific implementations
- Choice of implementation language and libraries
- Concrete operational infrastructure configuration (cloud products, middleware selection)
- Organization-specific deployment procedures and approval flows
- Compatibility and migration policy (specified in `compatibility.md`)

### 1.5 Relationship to the Requirements Definition Document

`requirements.md` is the higher-level document that defines the requirements to be satisfied, and this specification is the normative document that translates those requirements into an implementable language specification.
If there is a discrepancy between the requirements and this specification, the intent of the requirements document takes precedence and the specification is updated.

### 1.6 Terminology

This section defines the principal terms used in this specification.

- Function: An evaluation unit that receives input and returns a value. The basic unit of execution in this specification.
- Task: A colloquial term used in practice; in Lask, an execution target expressed as a function.
- Module: A source-file unit consisting of a set of declarations and imports.
- Execution environment: The target context in which commands are executed. Specified by environment expressions `#local` / `#docker(...)` / `#env(...)` (and the Docker sugar `#image-name`).
- Environment definition file: `environments.lask.json` (10.3), which defines named environments (remote connection targets, etc.).
- Dependency definition file: `dependencies.lask.json` (Chapter 5), which declares external Lask source code fetched over the internet (source location, version, and content hash).
- Execution event: A time-series record unit representing calls, return values, and failures.
- Serialization: The process of converting a value into an external representation so that it can be stored and transferred.
- Core expression: An expression remaining after static expansion of syntactic sugar (7.6). The evaluation target of the dynamic semantics (Chapter 8).
- Syntactic sugar: Surface syntax that is normalized into core expressions. Only the semantics of the expressions after normalization are normative (7.6).
- Core function: A built-in function that is the normalization target of syntactic sugar, or is integral to a language feature. It cannot be declared or overridden by user code (7.2).
- Built-in library: The set of built-in symbols available without explicit imports (Chapter 15).
- Pre-execution error: A failure detected and reported before function evaluation begins (environment resolution, argument binding, etc.).

## 2. Notation

This chapter defines only how to read the EBNF notation used throughout the specification document.

```ebnf
Production  = production_name "=" [ Expression ] "." .
Expression  = Alternative { "|" Alternative } .
Alternative = Term { Term } .
Term        = production_name | token [ "…" token ] | Group | Option | Repetition .
Group       = "(" Expression ")" .
Option      = "[" Expression "]" .
Repetition  = "{" Expression "}" .
```

How to read the EBNF:

- `A = B .` means "`A` is defined as `B`".
- `|` denotes alternation.
- `{ X }` denotes zero or more repetitions of `X`.
- `[ X ]` denotes that `X` is optional.
- `( X )` denotes grouping.
- `"..."` denotes a terminal symbol (a literal string).
- `production_name` denotes a nonterminal symbol (a reference to another production rule).

## 3. Lexical Specification

This chapter defines the lexical elements that constitute source code.

Common lexical elements:

- `unicode_char` is a terminal symbol representing any Unicode scalar value except a newline.
- `newline` is a newline (U+000A). Implementations may treat CRLF (U+000D U+000A) as a single newline.

### 3.1 Comments

This specification defines two kinds of comments: line comments and block comments.

```ebnf
line_comment  = "//" { unicode_char } .
block_comment = "/*" { unicode_char | newline | block_comment } "*/" .
```

Rules:

- Because `unicode_char` does not include newlines, a line comment ends immediately before a newline.
- Because a block comment may contain `newline`, it can span multiple lines.
- By including `block_comment` itself in the definition of `block_comment`, nesting of block comments is permitted.
- Permitting nesting allows temporarily commenting out a large region without breaking existing internal `/* ... */` comments.
- Implementations must track the comment depth and must determine comment termination only at the outermost `*/`.

### 3.2 Identifiers

```ebnf
identifier = lower_id | upper_id .
lower_id   = ( "a" … "z" | "_" ) { letter | decimal_digit | "_" } .
upper_id   = ( "A" … "Z" | "_" ) { letter | decimal_digit | "_" } .
letter        = "A" … "Z" | "a" … "z" .
decimal_digit = "0" … "9" .
```

Rules:

- Identifiers cannot contain `-`. Because `-` is always interpreted as a binary operator (6.2), `a-b` is unambiguously parsed as `a - b` (subtraction).
- Multi-word identifiers are composed in snake case using `_` (`show_version`).
- The name mapping for calling functions from the CLI with `-`-separated names (`show-version`) is defined in 11.2. Because identifiers cannot contain `-`, this mapping is collision-free.

### 3.3 Literals and Environment Tokens

```ebnf
literal     = null_lit | bool_lit | number_lit | string_lit .
null_lit    = "null" .
bool_lit    = "true" | "false" .
number_lit  = decimal_digit { decimal_digit } [ "." decimal_digit { decimal_digit } ] .
string_lit  = raw_string_lit | interpreted_string_lit .
raw_string_lit         = "'" { raw_char | newline } "'" .
interpreted_string_lit = '"' { string_char | interpolation } '"' .
interpolation          = "#{" Expression "}" .
env_head    = "#" env_char { env_char } .
env_char    = letter | decimal_digit | "/" | ":" | "." | "-" | "_" | "@" .
```

`env_head` is a lexical token dedicated to environment expressions (6.7) that is not included in `literal`; this section defines only its lexical rules.

Among string literals, `raw_string_lit` is called a raw string and `interpreted_string_lit` is called an interpreted string.

String character classes and termination rules:

- `raw_char` is a `unicode_char` excluding `'`. A raw string may contain newlines (`newline`), but no means is provided to include `'` itself. When a string containing `'` is needed, use an interpreted string.
- `string_char` is a `unicode_char` excluding `"`, `\`, and newlines, or an escape sequence. A newline cannot be included directly in an interpreted string; use `\n` instead.
- A string literal terminates at the first matching closing quote that appears.

Lexical rules:

- Whitespace (spaces, tabs) is treated as a token separator except inside string literals, command strings (6.6), and comments.
- Consecutive whitespace is treated as equivalent to a single separator.
- A newline is treated as an independent token (`newline`); at the top level it marks the end of a declaration (the declaration termination rules of Chapter 5), inside a `do` block it marks the end of a statement (the statement termination rules of 6.5), and in a command execution expression it marks the end of the command string (6.6). However, a newline that falls under the continuation rules of 6.5 is ignored in the same way as token-separating whitespace.
- Consecutive newlines (blank lines) are treated as equivalent to a single newline token. Blank lines produce no declarations or statements.
- Comments are treated the same as whitespace for the purposes of lexical analysis.
- A line comment ends at a newline, and a block comment is enclosed by `/*` and `*/`.
- Number literals do not include a sign. `-` and `+` are always interpreted as binary operators. Negative numbers cannot be expressed as literals and are constructed with expressions such as `0 - 1` (unary minus is not provided).
- `env_head` is the leading token of an environment expression (6.7) and is read with longest match. It terminates at the position where a character that is not an `env_char` (whitespace, `]`, `,`, `)`, `}`, etc.) appears.
- A `#` not followed by at least one `env_char` is a lexical error (except for the interpolation opener `#{` inside interpreted strings and command strings).
- The character set of `env_char` is defined so that any Docker image reference including registry, repository, tag, and digest can be written. `@` is included for digest specification (e.g. `#alpine@sha256:...`).

Reserved words:

- The following words are reserved words and must not be used as identifiers.
- `import`, `from`, `as`, `type`, `do`, `async`, `await`, `if`, `else`, `for`, `return`, `try`, `catch`, `finally`, `true`, `false`, `null`
- `stdin` is not a reserved word but is a reserved identifier (9.3), and must not be declared or rebound in user code.

String interpolation:

- String interpolation can be used only in interpreted strings (`"..."`).
- Interpolation starts with `#{` and ends with the matching `}`.
- The interpolated part is lexed and parsed as an `Expression`.
- Raw strings (`'...'`) do not perform interpolation; their contents are treated as the string value as-is.

Escape sequences:

- Escape sequences are valid only in interpreted strings.
- The minimum supported escapes are `\\`, `\"`, `\n`, `\r`, `\t`.
- Unicode escapes are supported in the `\u{HEX}` form (`HEX` is one or more hexadecimal digits).
- Raw strings perform no escape processing.

## 4. Type System

This chapter defines the types of values that Lask handles, type annotations, type inference, and type compatibility.

### 4.1 Classification of Types

- Base types
  - `Any`: A general-purpose type that accepts any value
  - `Number`: Integers and floating-point numbers
  - `String`: UTF-8 strings
  - `Bool`: Boolean values
  - `Null`: Absence of a value
  - `Void`: Absence of a return value
  - `Environment`: The execution environment of commands (Chapter 10). It holds a special position distinct from the other base types in that its values are constructed only by environment expressions (6.7) and its serialization is limited to a metadata representation (4.5, 13.1).
- Composite types
  - `Array<T>`: An array whose element type is `T`
  - `Map<T>`: A map with string keys and value type `T`
  - `Record<field1: T1, field2: T2, ...>`: A record with fixed fields
  - `AsyncHandle<T>`: A handle referring to the result `T` of an asynchronous execution
  - `Function<T1, T2, ..., R>`: A function type consisting of positional parameters only. The last type argument is the return type.

### 4.2 Type Syntax

```ebnf
Type              = BaseType | ArrayType | MapType | RecordType | AsyncHandleType | FunctionType | NamedType .
BaseType          = "Any" | "Number" | "String" | "Bool" | "Null" | "Void" | "Environment" .
ArrayType         = "Array" "<" Type ">" .
MapType           = "Map" "<" Type ">" .
RecordType        = "Record" "<" [ RecordFieldType { "," RecordFieldType } ] ">" .
RecordFieldType   = ( lower_id | string_lit ) ":" Type .
AsyncHandleType   = "AsyncHandle" "<" Type ">" .
FunctionType      = "Function" "<" Type { "," Type } ">" .
NamedType         = upper_id .
TypeAliasDecl     = "type" upper_id "=" Type .
```

Representation of Function types:

- `Function<T1, T2, ..., R>` denotes a function type that takes `T1, T2, ...` in positional order and returns `R`.
- `AsyncHandle<T>` denotes a reference to a result `T` of an asynchronous execution that is in progress or completed.
- `Function<R>` denotes a function that takes no arguments.

Definition of named types:

- Named types are defined by the type alias declaration `type`.
- The form is `type TypeName = Type`.
- Example: `type Strings = Array<String>`

Record field names:

- Field names may be written in identifier form (`lower_id`) or string literal form (`string_lit`). Identity is determined by comparison as strings, and `Record<name: String>` and `Record<"name": String>` denote the same type.
- Names that do not conform to `lower_id` (e.g. `"APP_ENV"`, `"X-Api-Key"`) can be written only in string literal form.
- String literals used in type notation and as object literal keys must not contain interpolation (`#{...}`).
- Duplicate field names within the same record type or the same object literal (including collisions between identifier form and string literal form) are a static error (`E-TYPE-FIELD-DUPLICATE`).
- Access to field names that do not conform to `lower_id` uses string literal indexing (6.8).

Type well-formedness rules:

- `Void` may appear only in the return-value position of a function type (`Function<..., Void>`) and as the type argument of `AsyncHandle<Void>`. Its appearance as a type argument of `Array` or `Map`, as a record field type, or in a parameter position of a function type is a static error (`E-TYPE-ILLFORMED`).
- All types constructed during type checking, including instantiations of the type variables of built-in polymorphism (4.4), must satisfy these well-formedness rules (e.g. an instantiation making the result type of `map` be `Array<Void>` is `E-TYPE-ILLFORMED`; constructing `AsyncHandle<Void>` via `spawn` is legal).
- A type alias (`TypeAliasDecl`) must not reference itself directly or indirectly (recursion and mutual recursion are prohibited). A violation is a static error (`E-TYPE-ILLFORMED`). This rule guarantees that expansion of `NamedType` (4.3) always terminates in a finite number of steps.

Examples:

- Base types
  - `Number`
  - `String`
  - `Environment`
- Array types
  - `Array<Number>`
  - `Array<Record<name: String, age: Number>>`
- Map types
  - `Map<String>`
  - `Map<Array<Number>>`
- Record types
  - `Record<name: String, age: Number>`
  - `Record<env: Environment, tags: Array<String>>`
  - `Record<"APP_ENV": String, "REGION": String>`
- Function types
  - `Function<Number, Number, Number>`
  - `Function<String, Record<message: String, ok: Bool>>`
  - `Function<Number>`
- Named types
  - `Strings` (`type Strings = Array<String>`)
  - `User`
  - `BuildConfig`

### 4.3 Type Annotations and Inference

Type annotations are optional, and the following type inference rules apply where annotations are absent.

- When an annotation is given, the annotation takes precedence, and the inferred type must conform (per the conformance relation of 4.4) to the annotated type.
- Type annotations (on variables, parameters, and return values) and the argument types of a callee propagate as the expected type for the corresponding expression, and are used for the expected-type-directed checking of literals (described below).
- A number literal is `Number`, a string literal is `String`, a boolean literal is `Bool`, and the `null` literal is `Null`. An environment expression `#...` (6.7) is `Environment`.
- For an array literal `[e1, e2, ...]` with no expected type, the type of each element is inferred; if all are the same type `T`, the literal is `Array<T>`, and if heterogeneous, it is `Array<Any>`.
- When the expected type is `Array<T>`, if every element expression conforms to `T`, the literal's type is `Array<T>`. If any element does not conform, it is a type error (conformance is invariant (4.4), but literals can promote element-wise to `Any` etc. via expected-type-directed checking).
- The empty array `[]` follows the expected type if one exists; with no expected type it is `Array<Any>`.
- For an object literal `{k1: e1, ...}` with no expected type, it is inferred as `Record<k1: T1, ...>` (`Ti` is the inferred type of each `ei`). When a key is a string literal, that string is the field name (per the record field name rules of 4.2).
- An object literal with an expected type is checked for consistency with the expected type by the following rules.
  - When the expected type is `Record<...>`: the literal's key set must match the expected type's field set (key identity follows the field name rules of 4.2), and each value expression must conform to the corresponding field type. Missing or extra keys and type nonconformance are type errors.
  - When the expected type is `Map<T>`: when every value expression conforms to `T`, the literal's type is `Map<T>`. If any value does not conform, it is a type error (keys are always treated as `String`. 4.4).
  - When the expected type is `Any`: inferred as `Record<...>` in the same way as when there is no expected type.
  - Any other expected type is a type error.
- When a variable or function declaration has no annotation, the type of the right-hand-side expression is adopted as the declared type.
- When a function has no return-value annotation, the type of the function body is inferred as the return type.
- When a parameter has no annotation, `Any` is adopted.
- In a function call `f(a1, ..., an)`, `f` must resolve to `Function<T1, T2, ..., Tm, R>`, the argument binding must satisfy the rules of 7.5 (including keyword arguments and default-value completion), and each bound argument must conform to the type at the corresponding position. The result type is `R`.
- In the form `Function<T1, T2, ..., R>`, the last type argument is the return type, and the preceding ones are the positional parameter types.
- A `NamedType` is expanded into its `TypeAliasDecl` before type checking, and inference is performed on the expanded type.
- Overloading is not supported. One symbol has one function type.
- Abstract types (type classes, etc.) are not supported. Declaring type variables and polymorphic type annotations in user code are also not supported. Only built-in symbols follow the built-in polymorphism rules of 4.4.

Diagnostic rules on inference failure:

- If the type of an expression does not conform to the annotated type, it is a type error.
- If argument binding at a call does not satisfy the rules of 7.5, or an argument type does not conform, it is a type error.
- If multiple constraints conflict and cannot be unified into a single type, it is a type error.
- If a single final type cannot be determined, it is an inference failure error.

Examples of type inference:

- `n = 1` is inferred as `n: Number`.
- `ok = true` is inferred as `ok: Bool`.
- `names = ["a", "b"]` is inferred as `names: Array<String>`.
- `mixed = [1, "a"]` is inferred as `mixed: Array<Any>`.
- `empty = []` is inferred as `empty: Array<Any>` when there is no expected type.
- `user = {name: "alice", age: 20}` is inferred as `user: Record<name: String, age: Number>`.
- `envMap: Map<String> = {"APP_ENV": "prod", "REGION": "ap-northeast-1"}` is typed as `Map<String>` due to the expected type `Map<String>`.
- `hdrs = {"X-Api-Key": "secret"}` is inferred as `hdrs: Record<"X-Api-Key": String>` because there is no expected type.
- `bad: Map<Number> = {"a": "x"}` is a type error because the value `"x"` does not conform to `Number`.
- For `add(x: Number, y: Number) = x + y`, the call `add(1, 2)` is inferred to have result type `Number`.
- `identity(x) = x` is inferred as `identity: Function<Any, Any>` because the parameter has no annotation and no default value.
- For `inc(--x = 0) = x + 1`, the keyword parameter `x: Number` is inferred from the default value `0`, and `inc` is inferred as `inc: Function<Number>` (no positional parameters). The calls `inc()` and `inc(x = 5)` are both legal, with result type `Number`.
- Given `type Strings = Array<String>`, `xs: Strings = ["a"]` is treated as equivalent to `xs: Array<String>`.

### 4.4 Type Semantics

Type conformance relation:

"Conformance" in type checking is a binary relation defined as follows. When the type `T` of an expression conforms to the type `U` of a required position, that expression may be placed in a position requiring `U` (an argument, the right-hand side of an annotated declaration, a field value, a return value, etc.).

- Reflexivity: `T` conforms to `T`. Type identity is determined by structural identity after expanding `NamedType` (4.3).
- Top type: any `T` conforms to `Any`. `Any` is the only top type.
- No conformance relation exists for any pair of types other than the above.

Invariance (no variance):

- Conformance is not lifted into the interior of type constructors (invariant). `Array<Number>` does not conform to `Array<Any>` (element types must be identical). The same applies to `Map` and `AsyncHandle`.
- `Record` has no width or depth subtyping. It conforms only when the field set and each field type are identical.
- Function types conform only when identical. No variance (contravariance or covariance) is introduced for argument positions or return-value positions.
- Under this definition, the soundness conditions of variance associated with subtyping (contravariance in function argument positions, etc.) do not arise.

The meaning of types is defined by the following rules.

- `Any` is the only top type that accepts values of all types (any type conforms to `Any`).
- An expression of type `Any` may be placed only in positions requiring `Any`. Moving from `Any` to a concrete type can be done only via `cast` (15.8), which involves a runtime type check. No static implicit conversion is provided.
- `Null` is a value type representing the absence of a value and can be evaluated as an expression.
- `Void` is a type representing a computation result that returns no value, and must not be used as a value.
- `Null` and `Void` are not identified with each other. Placing `Void` in a position requiring `Null`, or vice versa, is a type mismatch.
- `Array<T>` denotes an array whose elements are uniformly `T`.
- An array with mixed element types is treated as `Array<Any>`.
- `Map<T>` is a built-in composite type denoting a dynamic associative array with string keys; the key type is always fixed to `String`.
- `Record<...>` denotes a structure whose field set and field types are statically fixed.
- The division of roles is: `Map<T>` for structures whose key set may vary at runtime, and `Record<...>` for structures with a key set fixed by specification.
- `AsyncHandle<T>` is a runtime handle managing the lifecycle of an asynchronous computation, and is resolved to `T` by `await`.
- Conformance between function types `Function<T1, T2, ..., R>` follows the conformance relation at the beginning of this section, and holds only when the number of positional parameters, each parameter type, and the return type are all identical (promotion to `Any` occurs only in positions requiring a top-level `Any`).
- Parameter names, keyword parameters, and variadicity are not part of a function type. Because declared parameter information cannot be consulted when a function is applied after being passed around as a function value, binding is performed with positional arguments only, and all keyword parameters are completed with their default values (7.5).
- Because overloading is not permitted, multiple function types must not be assigned to a single function symbol at the same time.
- A `NamedType` is treated as equivalent to the type obtained after expanding the corresponding `TypeAliasDecl`.

Polymorphic types of built-in symbols (built-in polymorphism):

- The type syntax of this specification (4.2) has no type variables, and user code cannot declare polymorphic types. An `upper_id` appearing in a type annotation in user code is always resolved as a `NamedType` (a type alias reference), and if there is no corresponding declaration, it is an undefined reference error.
- However, only built-in symbols defined by this specification (core functions and functions of the built-in library; Chapters 6 and 15) may have polymorphic types whose signatures contain type variables (single uppercase names such as `T`, `U`, `R`).
- A signature containing type variables is treated as a type scheme, and is instantiated to concrete types independently for each call and then checked. Within a single call, type variables of the same name must be bound to the same concrete type.
- Instantiation is determined from the argument types and the contextual expected type. If a single concrete type cannot be determined, it is an inference failure error (4.3).
- When a built-in polymorphic function is referenced as a function value in a position other than a call (binding to a variable, passing as an argument, etc.), the type variables must be uniquely instantiable from the expected type at the reference position. If instantiation is not possible, it is a type error. Example: `m: Function<Array<Number>, Function<Number, Number>, Array<Number>> = map` can be instantiated via the expected-type annotation, but `m = map` without an annotation is an inference failure error.
- Implementations may realize built-in polymorphism by any internal mechanism, but must satisfy the observable type-checking results above. Polymorphism must not be extended to user-defined symbols.

### 4.5 Serializable and Non-serializable Types

- Directly serializable
  - `Any`, `Number`, `String`, `Bool`, `Null`
  - `Array<T>`, `Map<T>`, `Record<field1: T1, field2: T2, ...>`
- Not directly serializable
  - `Void`, `Environment`
  - `AsyncHandle<T>`
  - `Function<T1, T2, ..., R>`
- Function values are converted to reference metadata (`FunctionRef`; 13.2) in execution records and external output.
- Likewise, `Void` and `Environment` are converted to a tagged metadata representation (13.1) in external output. Restoration by deserialization is not guaranteed for either (the round-trip rules of 13.1).

## 5. Declarations and Modules

This chapter defines modules, declarations, public symbols, and imports.

```ebnf
Module        = { TopLevelDecl } .
TopLevelDecl  = ( ImportDecl | TypeAliasDecl | Declaration ) decl_end .
decl_end      = newline | ";" .
TypeAliasDecl = "type" upper_id "=" Type .
Declaration = ValueDecl | FunctionDecl .
ValueDecl   = lower_id [ ":" Type ] "=" Expression .
FunctionDecl = lower_id "(" [ FunctionParameterList ] ")" [ ":" Type ] "=" Expression .

ImportDecl      = "import" ( NamedImports | NamespaceImport ) "from" ImportPath .
NamedImports    = "{" ImportSpecifier { "," ImportSpecifier } "}" .
ImportSpecifier = ( lower_id | upper_id ) [ "as" ( lower_id | upper_id ) ] .
NamespaceImport = "*" "as" lower_id .
ImportPath      = string_lit .
```

Declaration termination rules:

- A top-level declaration terminates at a newline, `;`, or end of file. Immediately before the end of file, `decl_end` may be omitted.
- Termination and continuation are determined identically to the statement termination rules of 6.5 (continuation tokens, continuation by line-leading tokens, and the handling of line-leading `(` and `[`). By the continuation rules, the right-hand side of a declaration can span multiple lines.
- `;` is an optional separator for writing multiple declarations on one line, and is not required at the end of a line.
- `as` and `from` are not continuation tokens (6.5). An `import` declaration must be written on one line, except inside the braces of `NamedImports` (which may span multiple lines by the open-bracket continuation rule). The closing `}` and the `from` clause must be placed on the same line.
- A line-leading `(` or `[` does not continue the preceding declaration and is interpreted as the start of a new declaration. Since a top-level declaration begins with `import`, `type`, or an identifier, this case results in a syntax error.

Module loading unit:

- One source file is regarded as one module.
- A module can depend on other modules via `import` declarations.

Import path resolution:

- An `ImportPath` starting with `./` or `../` is a local import. It is resolved relative to the directory of the importing module.
- Any other `ImportPath` (a bare path) is an external import. Its first path segment must be a dependency name declared in the dependency definition file (see "External dependencies" below); otherwise it is a static error (`E-MODULE-UNRESOLVED`).
- If the dependency is a single `.lask` file, the dependency name itself is the module path (e.g. `import { send } from "notify"`).
- For a dependency fetched as a source tree (`git`, or an archive `url`), the remainder of the path is resolved within the fetched tree (e.g. `import { rollout } from "deploy-kit/deploy.lask"`).
- A bare dependency name with no path remainder resolves to `main.lask` at the root of the fetched tree (the entry-point convention). Publishers should expose the public API of a tree dependency from its `main.lask`. If the tree has no `main.lask`, the bare-name import is a static error (`E-MODULE-UNRESOLVED`).

Public scope and visibility:

- A `Declaration` or `TypeAliasDecl` declared at the top level is a public symbol of that module.
- Local bindings defined inside a module (function parameters, bindings inside `do`, etc.) are not public.
- The same top-level symbol name must not be declared more than once within the same module.

Import forms:

- A named import `import { a, b } from "path"` brings only the specified names, among the public symbols of the target module, into the name resolution scope of the current module. If a specified name does not exist among the public symbols, it is an error before type checking.
- `import { a as b } from "path"` brings in the public symbol `a` under the name `b` (renaming import). The identifiers before and after renaming must be of the same kind (`lower_id` with `lower_id`, `upper_id` with `upper_id`).
- A namespace import `import * as m from "path"` refers to the target module through the namespace `m`. Public symbols are not brought in implicitly and are referenced in the form `m.symbol`.
- Type aliases (`upper_id`) can be brought in by named import. Because a type reference in a type annotation is limited to a `NamedType` (4.2) consisting of a single `upper_id`, type references via a namespace (`m.TypeName`) are not possible. To use a type, use a named import.
- No form is provided that unconditionally brings in all public symbols (the names to be brought in are made explicit in the declaration).

Evaluation timing of top-level declarations:

- A function declaration defines a closure and evaluates nothing at load time.
- The right-hand side of a top-level value declaration is evaluated lazily: at most once, on first reference, and the result is cached thereafter. A failure during this evaluation propagates from the referencing site.
- Consequently, importing a module never executes commands or produces effects by itself. Evaluation occurs only when a function is invoked or a value is referenced.

Rules for same-name symbol collisions:

- If a top-level declaration of the current module and a symbol brought in by a named import (under its post-renaming name) have the same name, it is an error before type checking.
- If the post-import names of named imports collide with each other, it is an error before type checking.
- If a namespace name of a namespace import is duplicated, or has the same name as a top-level declaration or an already-imported symbol, it is an error before type checking.

Handling of circular imports:

- The module dependency graph must be a directed acyclic graph (DAG). This requirement extends across external dependencies.
- If a circular import is detected, it is a module resolution error before execution.

External dependencies:

External Lask source code fetched over the internet is declared in the dependency definition file and imported by name. Code contains only the dependency name (intent); the source location, version, and integrity information are bound in the project file.

Dependency definition file:

- The default file name is `dependencies.lask.json`, loaded from the base directory for module resolution (the same location as `main.lask`). No per-invocation override of this file is provided (dependencies must be identical for every invocation).
- Schema:

```json
{
  "dependencies": {
    "deploy-kit": {"git": "https://github.com/example/lask-deploy-kit", "rev": "v1.2.0", "hash": "sha256-..."},
    "notify": {"url": "https://example.com/tasks/notify.lask", "hash": "sha256-..."}
  }
}
```

- The top-level `dependencies` map associates a dependency name (a string conforming to `lower_id`; 3.2) with an entry.
- An entry has exactly one source: `git` (a repository URL; `rev` — a tag or commit — is required) or `url` (an archive or a single `.lask` file).
- `hash` (a content hash of the fetched source) is required for every entry. The same dependency definition must always yield identical source code.
- Entries are typically recorded with `lask deps add` (11.5), which fetches the source and computes the content hash on first use.
- Secrets (credentials, tokens) must not be written in this file.

Fetching and verification:

- Dependencies are fetched into an implementation-defined cache and verified against `hash` by the CLI (`lask deps sync`; 11.5). A verification failure is `E-MODULE-HASH-MISMATCH`.
- `check`, `run`, `eval`, and `envs` must not access the network for module resolution. If a declared dependency is not present in the cache, or fails verification, it is a static error (`E-MODULE-UNRESOLVED`).
- An external module may itself have a `dependencies.lask.json`. Transitive dependencies are resolved independently per dependency (no version unification is performed; duplication across the dependency graph is permitted).

Execution environments in external modules:

- Only the environment definition file of the root project is in effect (10.3). `#env(name)` in an external module resolves against the root project's environment definitions; an undefined name is a static error. Environment definition files carried by external modules have no effect.

Examples:

```lask
// module: lib/math.lask
add(x: Number, y: Number): Number = x + y
```

```lask
// module: lib/types.lask
type Strings = Array<String>
joinWithComma(xs: Strings): String = join(xs, ",")
```

```lask
// module: app/main.lask
import { add } from "./lib/math.lask"
import * as types from "./lib/types.lask"
import { send } from "notify"

sum2(a: Number, b: Number) = add(a, b)
labels = ["a", "b", "c"]
csv = types.joinWithComma(labels)
notify_all(): Void = forEach(labels, \(x) -> send(x))
```

Here `notify` is an external dependency declared in `dependencies.lask.json` as a single-file source.

## 6. Expressions

This chapter defines the expressions of Lask. Every unit of execution in Lask is expressed as an expression.

```ebnf
Expression   = UnaryExpr | Expression binary_op Expression .
UnaryExpr    = PrimaryExpr | unary_op UnaryExpr .
PrimaryExpr  = operand | AccessorExpr | CallExpr | DoExpr | IfExpr | ForExpr | TryExpr | AsyncExpr | AwaitExpr .

operand      = literal | lower_id | array_lit | object_lit | "(" Expression ")" | LambdaExpr | CommandExpr | EnvExpr .
array_lit    = "[" [ Expression { "," Expression } ] "]" .
object_lit   = "{" [ KeyValuePair { "," KeyValuePair } ] "}" .
KeyValuePair = ( lower_id | string_lit ) ":" Expression .

AccessorExpr = PrimaryExpr "." lower_id | PrimaryExpr "[" Expression "]" .
CallExpr     = PrimaryExpr "(" [ Arguments ] ")" .
Arguments    = Argument { "," Argument } .
Argument     = [ lower_id "=" ] Expression .
```

### 6.1 Function Parameters, Lambda Expressions, and Higher-Order Functions

```ebnf
FunctionParameterList = PositionalParameters [ "," VariadicParameter ] [ "," KeywordParameters ]
                      | VariadicParameter [ "," KeywordParameters ]
                      | KeywordParameters .
PositionalParameters  = PositionalParameter { "," PositionalParameter } .
PositionalParameter   = lower_id [ ":" Type ] .
VariadicParameter     = "..." lower_id [ ":" ArrayType ] .
KeywordParameters     = KeywordParameter { "," KeywordParameter } .
KeywordParameter      = "--" lower_id [ ":" Type ] "=" Expression .
LambdaExpr            = "\\" "(" [ FunctionParameterList ] ")" [ ":" Type ] "->" Expression .
```

Meaning of the parameter notation:

- At declaration time, each parameter is clearly distinguished as a positional parameter (`name`), a variadic parameter (`...name`), or a keyword parameter (`--name`). The declaration order is: the sequence of positional parameters, the variadic parameter (at most one), then the sequence of keyword parameters (enforced by the grammar).
- Positional parameters are bound only by positional arguments. All are required and cannot have default values.
- Keyword parameters are bound only by name (`name = expression` in in-language calls, `--name <value>` on the CLI; 11.2). They must have a default value expression, and when unspecified they are completed with the default value.
- Binding a positional parameter by name, and binding a keyword parameter by position, are not possible (`E-TYPE-KEYWORD`).
- `--` is a lexeme that appears only as the declaration marker of a keyword parameter, and is not interpreted as a sequence of the operator `-`.
- Parameter names are used for references within the function body and for the binding interface of keyword parameters (references within the body use `name` for every kind).

Keyword arguments:

- A call argument can specify a keyword parameter in the form `name = expression` (`Argument = [ lower_id "=" ] Expression`; beginning of Chapter 6).
- Positional arguments must be placed before all keyword arguments.
- The name of a keyword argument must match a keyword parameter name of the callee (the declaration parameter information of 7.5). Unknown names, naming of positional or variadic parameters, and duplicate bindings to the same parameter are static errors (`E-TYPE-KEYWORD`).
- Keyword arguments can be used only when the callee resolves statically to a function declaration or a lambda expression. Calls that resolve only to a value of function type (7.5) cannot consult the declaration parameter information, so they accept only positional arguments, and all keyword parameters are completed with their default values.
- Keyword arguments may be written in any order (e.g. `f(1, c = 3, b = 2)`).

Variadic parameters:

- Immediately after the sequence of positional parameters, at most one variadic parameter `...name` may be declared (keyword parameters are placed after it).
- The type annotation of a variadic parameter must be of the form `Array<T>`. If there is no annotation, it is `Array<Any>`. Within the function body it is referenced as a value of type `Array<T>`.
- A variadic parameter cannot have a default value expression. If zero corresponding positional arguments are given, the empty array `[]` is bound.
- The positional arguments remaining after all positional parameters have been bound are collected in written order and bound as one array. Each argument must conform to the element type `T`.
- A variadic parameter cannot be bound by a keyword argument (`E-TYPE-KEYWORD`).
- In the function type, a variadic parameter appears as one parameter of the corresponding array type (e.g. `sum(...xs: Array<Number>): Number` is `Function<Array<Number>, Number>`). Variadicity is not part of the function type and is retained as declaration parameter information (7.5). Therefore, in application via a function value (7.5), an array is passed explicitly as one positional argument.

Examples:

```lask
sum(...xs: Array<Number>): Number =
  reduce(xs, 0, \(acc: Number, x: Number) -> acc + x)

s1 = sum()
s2 = sum(1, 2, 3)

f: Function<Array<Number>, Number> = sum
s3 = f([1, 2, 3])
```

Examples:

```lask
greet(name: String, --prefix: String = "hello"): String =
  concat(prefix, concat(", ", name))

g1 = greet("alice")
g2 = greet("alice", prefix = "hi")
```

Desugaring rules for function declarations:

- `name(p1, p2, ..., pn) = body` is equivalent to `name = \(p1, p2, ..., pn) -> body`.
- The parameter specification of `FunctionDecl` follows the same rules as the `FunctionParameterList` of `LambdaExpr`.
- Therefore, the notation of including parameters in a declaration is syntactic sugar for the notation of assigning a lambda expression as a value.

Equivalence example:

```lask
f(x) = x + 1
g = \(x) -> x + 1
```

The `f` and `g` above declare essentially the same function.

### 6.2 Operators

```ebnf
unary_op  = "!" .
binary_op = "*" | "/" | "+" | "-" | "==" | "!=" | "<" | "<=" | ">" | ">="
          | "&&" | "||" | "|>" | "<|" | ">>" | "<<" .
```

Operators are syntactic sugar for writing function application readably. Operators themselves cannot be user-defined, and the evaluator gives them fixed meanings.

Operator precedence, from highest to lowest, is as follows.

1. Unary operator `!`
2. Multiplicative operators `*` `/`
3. Additive operators `+` `-`
4. Comparison operators `==` `!=` `<` `<=` `>` `>=`
5. Logical operators `&&` `||`
6. Pipe and composition operators `|>` `<|` `>>` `<<`

Operators of the same precedence associate from left to right.

The meaning of each operator is as follows.

- `!e` denotes Boolean negation.
- `e1 * e2`, `e1 / e2`, `e1 + e2`, `e1 - e2` denote arithmetic operations.
- `e1 == e2`, `e1 != e2` denote value comparison.
- `e1 < e2`, `e1 <= e2`, `e1 > e2`, `e1 >= e2` denote ordering comparison.
- `e1 && e2` does not evaluate the right-hand side when the left-hand side is false, and evaluates the right-hand side only when the left-hand side is true.
- `e1 || e2` does not evaluate the right-hand side when the left-hand side is true, and evaluates the right-hand side only when the left-hand side is false.
- `e1 |> f` is syntactic sugar for `f(e1)`.
- `f <| e1` is syntactic sugar for `f(e1)`.
- `f >> g` denotes function composition equivalent to `\(x) -> g(f(x))`.
- `f << g` denotes function composition equivalent to `\(x) -> f(g(x))`.

The types of the operators are as follows.

- `!` is typed as `Function<Bool, Bool>`.
- `*`, `/`, `+`, `-` are typed as `Function<Number, Number, Number>`.
- `<`, `<=`, `>`, `>=` are typed as `Function<Number, Number, Bool>`.
- `&&`, `||` are typed as `Function<Bool, Bool, Bool>`.
- `==`, `!=` are well-typed only when the types of the two sides match and that type is a comparable type; the result type is `Bool`.
- The comparable types are limited to `Number`, `String`, `Bool`, `Null`, `Environment`, and `Array<T>` / `Map<T>` / `Record<...>` whose element, value, and field types are all comparable types.
- Applying `==` / `!=` to types containing `Function`, `AsyncHandle`, `Void`, or `Any` is a static error. To compare values of `Any`, first move to a concrete type with `cast` (15.8) and then compare.
- Equality of structured values (`Array` / `Map` / `Record`) is determined by recursive structural comparison of elements, keys, and fields. Equality of `Environment` follows 8.8.
- `e1 |> f` is well-typed when `e1: T` and `f: Function<T, R>` hold, and the type of the whole expression is `R`.
- `f <| e1` is well-typed when `e1: T` and `f: Function<T, R>` hold, and the type of the whole expression is `R`.
- `f >> g` is well-typed when `f: Function<T, U>` and `g: Function<U, R>` hold, and the type of the whole expression is `Function<T, R>`.
- `f << g` is well-typed when `g: Function<T, U>` and `f: Function<U, R>` hold, and the type of the whole expression is `Function<T, R>`.

The pipe and composition operators are syntactic sugar for making the passing of functions readable from left to right, and parentheses can be used to make the association order explicit.

The result of comparison operators and logical operators is `Bool`. The result of arithmetic operators is `Number`, and they must be evaluable as numeric operations.
The arithmetic operators, including `+`, are exclusively for `Number`; applying them to `String` is a type error. String concatenation is not done with an operator; use the built-in library function `concat` (15.3).

### 6.3 Asynchronous Invocation and Awaiting

```ebnf
AsyncExpr = "async" UnaryExpr .
AwaitExpr = "await" UnaryExpr .
```

This section defines the syntactic sugar `async` for asynchronous launching, and the built-in core function `await` representing waiting.

Definitions as functions:

- `spawn` is a core function that asynchronously launches the zero-argument function received as its argument and returns a result handle.
- `await` is a core function that receives a result handle and joins to the completion value.

Syntax rules:

- `async e` is syntactic sugar for `spawn(\() -> e)`. Because `async` needs to delay the evaluation of its operand `e`, it is provided as syntactic sugar rather than as a function.
- `await e` is a parenthesis-omitting form applying the core function `await` to the operand `e`. Writing `await (e)` is also parsed by the same rule as an `AwaitExpr` with a parenthesized operand, and the meaning is the same.
- The operand of `AsyncExpr` / `AwaitExpr` is a `UnaryExpr`. Therefore `await h |> f` is interpreted as `(await h) |> f`. To mean `await (h |> f)`, make it explicit with parentheses.
- `await` is a reserved word and can appear only in the form of an `AwaitExpr`. To pass it around as a function value, write `\(h) -> await h`.

Here `spawn` and `await` are core functions and must not be directly declared or overridden by user code.

Typing rules:

- When `e: T`, the type of `async e` is `AsyncHandle<T>`.
- When `f: Function<T>`, the type of `spawn(f)` is `AsyncHandle<T>`.
- When `h: AsyncHandle<T>`, the type of `await h` is `T`.
- Passing anything other than `AsyncHandle<...>` to `await` is a type error.

Evaluation rules:

- `async e` starts evaluation at the point of the call and immediately returns a handle value `AsyncHandle<T>` to the caller.
- `await h` suspends the current evaluation until `h` completes, and returns the result value after completion.
- Applying `await h` multiple times to the same handle returns the same completion result.
- If a failure occurs inside `async e`, that failure is re-raised at the time of `await h`.

The execution environment may choose the concurrency mechanism for `async` (threads, an event loop, a remote execution queue, etc.) as implementation-defined, but must satisfy the typing rules and evaluation rules above.

### 6.4 Control Structures

```ebnf
IfExpr  = "if" "(" Expression ")" Block "else" Block .
ForExpr = "for" "(" lower_id ":" Expression ")" Block .
Block   = "{" { DoStmt } "}" .
```

Control structures are provided as the block-form expressions `IfExpr` / `ForExpr`, and semantically they are handled by normalization to the core function `choose` and the collection function `map`. `if` / `for` are reserved words and can appear only in the expression forms of this section (the function-call forms `if(c, t, f)` / `for(xs, body)` are not provided).

`IfExpr` / `ForExpr` are `PrimaryExpr` (beginning of Chapter 6) and can appear in any position where an expression can be placed, such as the right-hand side of a binding, a function body, or an argument.

Definitions as functions:

- `choose` is a core function that receives a condition value and two branch functions and evaluates only one of them.
- Iteration over collections is expressed with the purpose-specific core functions `map` / `filter` / `reduce` / `forEach` (15.4).
  - `map` applies the body function to each element and returns the results as an array in the same order.
  - `filter` keeps, in the same order, only the elements for which the predicate returns true.
  - `reduce` aggregates a collection into a single value with an initial value and a folding function.
  - `forEach` applies the body function to each element, discards the results, and returns `Void` (iteration for side effects).

Desugaring rules:

- `if (c) { ... } else { ... }` is syntactic sugar for `choose(c, \() -> do { ... }, \() -> do { ... })`.
- `for (x : xs) { ... }` is syntactic sugar for `map(xs, \(x) -> do { ... })`. However, when the type of the body block is `Void` (including an empty block), it is syntactic sugar for `forEach(xs, \(x) -> do { ... })` (`Array<Void>` is not constructed; 4.4).
- For iteration for side effects where the result array is unnecessary, either make the body of type `Void` or use `forEach` directly.

Here `choose` is a core function that exists only to serve as a normalization target, and is not exposed in the built-in library (it cannot be referenced, declared, or overridden by user code). `map` / `filter` / `reduce` / `forEach` are functions of the built-in library (15.4) and at the same time core functions serving as normalization targets, and likewise must not be overridden.

Typing rules:

- `choose` is typed as `Function<Bool, Function<T>, Function<T>, T>`.
- `map` is typed as `Function<Array<T>, Function<T, U>, Array<U>>`.
- `filter` is typed as `Function<Array<T>, Function<T, Bool>, Array<T>>`.
- `reduce` is typed as `Function<Array<T>, U, Function<U, T, U>, U>`.
- `forEach` is typed as `Function<Array<T>, Function<T, U>, Void>` (the return type `U` of the body is arbitrary and the result is discarded. The type variable `U` is used rather than `Any` because conformance of function types is limited to identity (4.4), in order to accept bodies of arbitrary return type through instantiation of built-in polymorphism).
- An `IfExpr` is well-typed only when the condition expression is `Bool` and the types of the two branch blocks are the same `T`, and the type of the whole expression is `T`.
- An `if` without `else` is not an expression. It appears solely as the `return` guard statement (the `GuardStmt` of 6.5), only in statement position.
- A `ForExpr`, when the target expression is `Array<T>` and the type of the body block is `R` (`R` other than `Void`), has the type `Array<R>` as the whole expression.
- When the type of the body block is `Void`, the `ForExpr` is normalized to `forEach` and the type of the whole expression is `Void`.

Evaluation rules:

- An `IfExpr` first evaluates only the condition expression, and evaluates only the then block if true, or only the else block if false (following `choose` of 8.5).
- A `ForExpr` evaluates the target collection, then traverses the elements from left to right, applies the body block, and aggregates the results into an array in the same order (following `map` of 8.5).
- The interior of a `Block` follows the same statement rules as a `do` block (6.5), and the value of the block is the value of its final statement. The value of an empty block with no statements is `Void` (6.5).

### 6.5 Procedural Notation

```ebnf
DoExpr     = "do" Block .
DoStmt     = ( BindStmt | ExprStmt | ReturnStmt | GuardStmt ) stmt_end .
BindStmt   = lower_id "=" Expression .
ExprStmt   = Expression .
ReturnStmt = "return" Expression .
GuardStmt  = "if" "(" Expression ")" Block .
stmt_end   = newline | ";" .
```

Here `Block` is the block defined in 6.4 (`"{" { DoStmt } "}"`), and `newline` is the newline token defined in 3.3. Immediately before the block terminator `}`, `stmt_end` may be omitted.
The block forms of `if` / `for` are expressions (the `IfExpr` / `ForExpr` of 6.4), so no dedicated statement forms exist. They appear as an `ExprStmt` or as the right-hand side of a `BindStmt`.

Statement termination rules:

- A statement inside a `do` block terminates at a newline, `;`, or immediately before the block terminator `}`.
- `;` is an optional separator for writing multiple statements on one line, and is not required at the end of a line (writing it there is not an error).
- A newline that falls under any of the following is not regarded as statement termination and is treated as continuation of the statement.
  - A newline at a position where a bracket opened inside the statement (`(`, `[`, `{`) has not yet been closed
  - A newline at a position where the token at the end of the line is a continuation token. The continuation tokens are limited to the following: `=`, `,`, `:`, `->`, binary operators (`|>`, `+`, `==`, etc.; the `binary_op` of 6.2), `!`
  - A newline where the leading token of the next line is a binary operator (`|>`, `+`, `==`, etc.)
  - A newline where the leading token of the next line is `else` (continuation after the closing `}` of the then block of an `IfExpr`)
  - A newline where the leading token of the next line is `catch` or `finally` (continuation after the closing `}` of a block of a `TryExpr`)
- `as` and `from` are neither continuation tokens nor leading tokens of continuation. A line-leading `as` or `from` does not continue the preceding statement or declaration (see the constraints on `import` declarations in Chapter 5).
- A line-leading `(` or `[` is interpreted as the start of a new statement. The call argument list or index access of the preceding statement must not begin with a line-leading `(` or `[`.
- Because numeric literals do not include a sign (3.3), a line-leading `-` or `+` is always uniquely interpreted as continuation of the preceding statement (a binary operator).
- In the rules and examples from this section onward, the statement separator `;` may be read as a newline. The two are synonymous as statement termination.
- These rules (determination of termination and continuation) also apply identically to the termination of top-level declarations (the declaration termination rules of Chapter 5).

Procedural notation is syntactic sugar to ease gradual migration to the expression-centered core language.
`do` is syntactic sugar for sequential evaluation, and `if (...) { ... } else { ... }` / `for (...) { ... }` are normalized by the rules of 6.4 into expressions that use `choose` / `map`.

Purpose and design policy:

- While allowing a procedural appearance, the semantics are unified into function application and expression evaluation.
- `do` gives only sequential evaluation order and introduces no execution model of its own.
- Control is always lowered to functions (`choose` and collection functions such as `map`), avoiding duplication of evaluation rules.

Desugaring rules:

- The desugaring rules for `if (...) { ... } else { ... }` / `for (...) { ... }` follow 6.4.
- `do { e }` is equivalent to `e`.
- `do { v = e1; s2; ...; sn; }` is normalized to an expression that first evaluates `e1`, binds it to `v`, and then evaluates `s2 ... sn`.
- `do { e1; s2; ...; sn; }` is normalized to an expression that discards the value of `e1` and evaluates `s2 ... sn`.

Sequential execution sugar:

- `do { stmt1; stmt2; ...; expr; }` is syntactic sugar for sequential execution that evaluates each statement in order and returns the value of the final expression.
- Bindings inside `do` become visible in order and can be referenced by subsequent statements.

Early return (`return`):

- `return e` is a statement that skips evaluation of the remainder of the body of the innermost enclosing function (function declaration or lambda expression) and makes `e` the result of that function.
- `return` may be placed only in the following positions (return-permitted positions). Violation is a syntax error (`E-SYNTAX-RETURN-POSITION`).
  - A statement position of the `do` block that is the body of a function declaration or lambda expression
  - A `GuardStmt` placed in a return-permitted position, and statement positions inside each branch block of an `IfExpr` in statement position (applied recursively)
- Therefore, it cannot be used inside the body of `for`, inside each block of `try`/`catch`/`finally`, or inside `do` blocks appearing in expression positions such as the right-hand side of a binding or an argument.
- A `GuardStmt` (an `if` without `else`) is permitted only when the final statement of the block is a `ReturnStmt` (6.4).
- No statement of the same block may be placed after a `ReturnStmt` (unreachable; syntax error).

Continuation-distribution transformation (normalization):

`return` is normalized to an expression using `choose` by the following transformation. `C...` denotes the subsequent statements (the continuation) of the same block.

- `do { A...; return e }` is equivalent to `do { A...; e }`.
- `do { A...; if (c) { B...; return e }; C... }` is transformed into `do { A...; choose(c, \() -> do { B...; e }, \() -> do { C... }) }`.
- When a branch of an `if (c) { X... } else { Y... }` in statement position contains `return`, the subsequent statements `C...` are injected into both branches and the transformation is applied likewise (a branch ending in `return` discards the continuation; a branch not so ending has the continuation appended at its end).
- The transformation is applied recursively from the innermost block. No `return` remains in the transformed expression.

Typing rules (early return):

- The expression `e` of every `return e` and the final value of the function body must have the same type `T` (the return type of the function). The existing typing rules on the expression after the continuation-distribution transformation (the branch type agreement of 6.4) guarantee this.
- If a `GuardStmt` is placed as the final statement of a function body, the continuation is empty, so it is a type error except when the return type is `Void`.

Typing rules:

- A `BindStmt` is well-typed if its right-hand `Expression` is well-typed.
- An `ExprStmt` is well-typed at any type `T`, but contextually its value is not passed to the next statement.
- The type of the whole `do` is the type of the last statement (the trailing `ExprStmt` or an expression equivalent to it). If it has no statements, it is `Void`.
- Empty blocks (`do {}`, and including empty `Block`s of the `IfExpr` / `ForExpr` of 6.4) are permitted. The type of an empty block is `Void`, and its evaluation result is `Void`.
- Even when the right-hand side of a `BindStmt` has type `Void`, the binding itself is possible, but the bound name cannot be referenced as a value (4.4).
- The typing rules for `IfExpr` / `ForExpr` follow 6.4 (the same when they appear as the right-hand side of a `BindStmt` or as the final statement).

Evaluation rules:

- Statements inside `do` are always evaluated from top to bottom.
- If evaluation of the right-hand side of a `BindStmt` fails, the whole `do` fails at that point and subsequent statements are not evaluated.
- The evaluation rules for `IfExpr` / `ForExpr` follow 6.4.

Scoping rules:

- An identifier introduced by a `BindStmt` inside a `do` block is in effect within the same block after that statement.
- Bindings inside a block of `if` / `for` (6.4) do not leak outside that block.
- The iteration variable `x` of `for (x : xs)` is in effect only inside the body block.

Examples:

```lask
build() = do {
  image = "app"
  tag = "latest"
  $ docker build -t #{image}:#{tag} .
  "ok"
}

classify(n: Number) = do {
  if (n > 0) {
    "pos"
  } else {
    "non-pos"
  }
}

labels(xs: Array<String>) = do {
  for (x : xs) {
    concat("item:", x)
  }
}

publish(tag: String): String = do {
  if (tag == "") { return "skip: no tag" }
  r = $* ./release.sh #{tag}
  if (r.code != 0) { return r.stderr }
  "released"
}
```

### 6.6 Command Execution Expressions

```ebnf
CommandExpr  = "$" [ stream ] [ "[" Expression "]" ] shell_string .
stream       = "1" | "2" | "*" .
shell_string = { shell_char | interpolation } .
shell_char   = unicode_char .
```

Here `shell_char` is `unicode_char` (the common lexical element of Chapter 3; excluding newlines). A command string terminates at the end of the line (details follow the lexical rules of this section).

A command execution expression is syntactic sugar for concisely writing shell command execution.
Semantically it is handled by normalization to the built-in function `runCommand`. The stream specifiers `1`, `2`, and `*` derive from file descriptor numbers and select which part of the command's result is received (`1` = standard output, `2` = standard error, `*` = the entire result).

Definition as a function:

- `runCommand` is a core function that executes a command in the specified execution environment and returns the entire result (exit code, standard output, standard error).
- Its signature is `runCommand(cmd: String, --env: Environment = #local): CommandResult`. The execution environment is given by the keyword parameter `env`, and when unspecified it is completed with the default execution environment `#local` (10.1).
- `runCommand` succeeds regardless of the exit code as long as the command completes (8.7). Failures occur only for execution infrastructure faults (unresolvable environment, inability to connect, inability to launch the command, etc.).

The `CommandResult` type:

- `type CommandResult = Record<code: Number, stdout: String, stderr: String>` is defined as a built-in type alias. `CommandResult` must not be overridden by user-defined type aliases or value declarations.
- `code` is the exit code of the command, and `stdout` and `stderr` are the contents of the respective streams. The construction rules follow 8.7.

Desugaring rules:

- `$* cmd` is syntactic sugar for `runCommand("cmd")`.
- `$ cmd` and `$1 cmd` are syntactic sugar for the following expression (`r` is a fresh identifier that does not collide with others).

  ```lask
  do {
    r = runCommand("cmd")
    if (r.code == 0) { r.stdout } else { fail({code: r.code, message: r.stderr}) }
  }
  ```

- `$2 cmd` is syntactic sugar for the expression above with its success branch replaced by `r.stderr`.
- The environment specifications `$[env]`, `$1[env]`, `$2[env]`, and `$*[env]` pass `env = env` to the `runCommand` of the respective expanded form.
- Only core functions (normalization to `runCommand`, `fail`, and `choose`) and a record literal appear in the expanded forms, so the behavior can never be changed by user definitions.
- `#{e}` inside a `shell_string` is evaluated first as string interpolation, and the post-interpolation string is passed as the command body.

Lexical rules:

- The command string extends from immediately after `$` (immediately after the stream specifier if present, or after the closing `]` if an environment specification is present) to the end of the line. The newline is the terminator of the command string and is treated as the newline token of 3.3. The stream specifier and environment specification must follow `$` without intervening whitespace.
- Leading and trailing whitespace of the command string is removed, and internal whitespace is preserved as is.
- If the line ends with `\`, it is a continuation line; the `\` and the newline are removed and the next line is concatenated.
- Inside the command string, the shell's `;`, quotation marks, pipes, etc. can be used without escaping.
- `#{` starts an interpolation. To include a literal `#{`, write `\#{`. A `#` not followed by `{` is treated as an ordinary character.
- Inside `do`, the newline that terminates a command string simultaneously serves as statement termination (6.5).
- A command string always terminates at a newline, but the enclosing expression can be continued by a binary operator on the next line following the continuation rules of 6.5. For example, placing `|> trim` on the line after `$ git --version` is interpreted as `runCommand("git --version") |> trim`.
- All tokens on the same line after a command execution expression become part of the command string. Therefore, no subsequent tokens of the expression (closing brackets, etc.) can be placed on the same line. To process it inside an expression, first bind it and then use it, or call `runCommand` directly.

Here `runCommand` is a core function and must not be directly declared or overridden by user code (15.5).

Environment specification rules:

- `Environment` is a basic type defined in 4.1, and the responsibilities of execution environments are defined in Chapter 10.
- `env` is an expression of type `Environment`. It is usually constructed with an environment expression (6.7) — `#local`, `#docker(...)` (including the sugar `#image-name`), or `#env(...)` — but any expression that returns an `Environment` value (a variable reference, etc.) can be placed there.
- If resolution of `env` fails, it is a pre-execution error.
- The default execution environment (the environment used by `$ ...` / `runCommand`) is always `#local` (10.1).
- Detailed environment resolution rules and responsibilities follow Chapter 10.

Typing rules:

- `runCommand` is typed as `Function<String, CommandResult>` (the keyword parameter `env` does not appear in the function type; 7.5).
- As a result of the desugaring, the expression type of `$ ...`, `$1 ...`, and `$2 ...` is `String`, and the expression type of `$* ...` is `CommandResult`.
- If `env` does not conform to `Environment`, it is a type error.
- The interpolation `#{e}` must be of a stringifiable type. If it cannot be stringified, it is a type error.

Evaluation rules:

- Evaluation follows the rules for the desugared expression (8.3, 8.7). The command string (including interpolations, evaluated left to right) is fixed first, and then `env` is evaluated.
- `$* ...` returns a `CommandResult` after the command completes, regardless of the exit code.
- `$ ...` (`$1 ...`) and `$2 ...` return the contents of the selected stream when the exit code is `0`. When it is nonzero, the `fail` of the expanded form causes a failure, and an `Error` value (`code` = the command's exit code, `message` = the contents of standard error output) can be caught by `try`/`catch` (6.9). If not caught, the command's exit code becomes the process exit code as is (8.10, 11.3).
- The handling of standard input, standard output, and standard error follows the input/output contract of Chapter 9.

Notes on security and portability:

- When embedding interpolated values into a command, the implementation should provide a safe escaping strategy.
- Shell dialect differences are execution-environment dependent; the specification prescribes only the evaluation procedure of the command string.

Examples:

```lask
show_version() = do {
  v = $ git --version
  v
}

build_in_docker(env: Environment) = do {
  out = $[env] sh -lc 'echo inside-container'
  out
}

show_in_fixed_image() =
  $[#alpine:3.20] uname -a

lint(): String = do {
  r = $* golangci-lint run
  if (r.code == 0) { "clean" } else { r.stdout }
}
```

### 6.7 Environment Expressions

```ebnf
EnvExpr     = env_head [ EnvArgList ] .
EnvArgList  = "(" [ EnvArgument { "," EnvArgument } ] ")" .
EnvArgument = [ lower_id "=" ] Expression .
```

An environment expression constructs a command execution target (Chapter 10) as a value in the form `#environment-kind(arguments...)`, and the type of the expression is always `Environment`. The arguments for each environment kind are given against an environment constructor signature (10.2) defined with the same parameter rules as functions (6.1: positional binding and default values).

Syntax rules:

- `env_head` is the lexical token defined in 3.3: `#` followed by an environment kind name or a Docker image name.
- The opening `(` of `EnvArgList` must be placed immediately after `env_head` without intervening whitespace. A `(` with intervening whitespace is not part of the environment expression and is interpreted as an independent expression.
- When an `EnvArgList` is present, the body of `env_head` (the part excluding `#`) must be an environment kind name conforming to `lower_id`. If it does not conform (e.g. `#alpine:3.12(...)`), it is a syntax error.
- An `EnvArgument` is a positional argument (`Expression`) or a named argument (`lower_id "=" Expression`). Positional arguments must be placed before all named arguments.
- The argument of the environment kind `env` must be a string literal containing no interpolation (10.2).
- The rules for named arguments are the same as for keyword arguments in function calls (6.1, 7.5). The name must match a keyword parameter name of the environment constructor signature; naming a positional parameter and duplicate specification of the same parameter are static errors (`E-TYPE-ENV-CONSTRUCT` for environment expressions).
- Because `env_char` includes `.`, an accessor `.` cannot follow immediately after `env_head` (it is absorbed into the token). If processing is needed, first bind it to a variable.

Desugaring rules:

- When there are no arguments, the `EnvArgList` can be omitted. `#name` (whose body matches an environment kind name of 10.2) is syntactic sugar for `#name()`. Example: `#local` is equivalent to `#local()`.
- A `#image-name` whose body does not match an environment kind name is syntactic sugar for `#docker("image-name")` (Docker sugar). Example: `#alpine:3.12` is equivalent to `#docker("alpine:3.12")`.
- Therefore, to specify a Docker image with the same name as an environment kind name (e.g. `local`), the explicit form `#docker("local")` must be used.
- Environment kind names such as `local` / `docker` / `remote` are not reserved words. In positions without `#`, they can be used as ordinary identifiers.

Typing rules:

- The type of an `EnvExpr` is `Environment`.
- Arguments are checked against the environment constructor signature of 10.2 using the call consistency of 7.5 (argument binding, type conformance, default-value completion) and the named argument rules of this section.
- An unknown environment kind, a missing positional argument, or an unknown named argument is a static error (7.7).

Evaluation rules:

- Argument expressions are evaluated left to right in written order (positional arguments, then named arguments).
- The evaluation result is an `Environment` value consisting of the environment kind and the normalized parameter set. Runtime environment resolution follows 10.4.
- If evaluation of an argument expression fails, the whole environment expression fails.

Examples:

```lask
e1 = #local
e2 = #alpine:3.12
e3 = #docker("alpine:3.12", memory="4g")
e4 = #ubuntu@sha256:9cee2b382fe2412cd77d5d437d15a93da8de373813621f2e4d406e3df0cf0e7c
e5 = #env("ansible")

run_in(env: Environment): String =
  $[env] uname -a
```

### 6.8 Accessor Expressions

```ebnf
AccessorExpr = PrimaryExpr "." lower_id | PrimaryExpr "[" Expression "]" .
```

An accessor expression denotes field reference on a record (`.`) and index reference on arrays and maps (`[...]`).

Distinction from module namespace references:

- In the form `m.symbol`, when the leading `m` resolves as a namespace name of a namespace import (Chapter 5), the whole expression is handled as name resolution (the fourth priority of 7.2) and is not an accessor expression.
- If a local binding with the same name as the namespace name exists, the priority order of 7.2 applies (the local binding takes precedence, in which case it is type-checked as a field access).
- Any other `.` is interpreted as a field access.

Typing rules for field access:

- `e.f` is well-typed only when the type of `e` is `Record<..., f: T, ...>` (a record type having the field `f`), and the type of the expression is `T`.
- If the target record type does not have the field `f`, or if the target's type is other than `Record` (including `Map` and `Any`), it is a static error (`E-TYPE-ACCESS`). Values of `Any` are referenced after moving to a concrete type with `cast` (15.8).
- Only field names in identifier form (`lower_id`) can be referenced with `.f`. Field names not conforming to `lower_id` (4.2) are referenced with a string-literal index (described below).
- Value retrieval from a `Map` uses `[...]` or `get` (15.4). Because the field set of a `Record` is statically fixed (4.4), field access does not fail at runtime.

Typing rules for index access:

- `e[i]` is well-typed when `e: Array<T>` and `i: Number`, and the type of the expression is `T`. Indexing is zero-based.
- `e[k]` is well-typed when `e: Map<T>` and `k: String`, and the type of the expression is `T`.
- `e[k]` is also well-typed when the type of `e` is `Record<...>` and `k` is a string literal containing no interpolation whose string matches a field name; the type of the expression is that field's type. `e.f` and `e["f"]` refer to the same field.
- An index expression on a record other than a string literal, and a string literal that does not match any field name, are static errors (`E-TYPE-ACCESS`) (because the key set is statically fixed; use `Map` for dynamic keys).
- Any other combination of target type and index type is a static error (`E-TYPE-ACCESS`).

Evaluation rules:

- The evaluation procedure and failure contract (runtime errors for out-of-range indices and absent keys) are defined in 8.9.
- `m[k]` has the same failure contract as `get(m, k)`.

Examples:

```lask
user = {name: "alice", age: 20}
userName = user.name

xs = [10, 20, 30]
first = xs[0]

envMap: Map<String> = {"APP_ENV": "prod"}
appEnv = envMap["APP_ENV"]

hdrs = {"X-Api-Key": "secret"}
apiKey = hdrs["X-Api-Key"]
```

### 6.9 Error Handling Expressions

```ebnf
TryExpr       = "try" Block ( CatchClause [ FinallyClause ] | FinallyClause ) .
CatchClause   = "catch" "(" lower_id ")" Block .
FinallyClause = "finally" Block .
```

An error handling expression is an expression that catches failures during evaluation (runtime errors and external I/O errors) and joins to an alternative computation. Semantically it is handled by normalization to the core functions `recover` and `fail`.

The `Error` type:

- `type Error = Record<code: Number, message: String>` is defined as a built-in type alias. `Error` must not be overridden by user-defined type aliases or value declarations.
- `code` denotes an exit code and `message` a human-readable message. The mapping from failures to `Error` values (including the carrying over of a command's exit code and standard error) is defined in 8.10.

Definitions as functions:

- `recover` is a core function that receives a body function and a handler function, catches failures of the body, and joins to the handler.
- `fail` is a core function that receives an `Error` value and raises that failure. It is used for re-raising within a handler and for raising user-defined errors.
- `error` is a helper function for constructing error values (15.7).

Here `recover` and `fail` are core functions and must not be directly declared or overridden by user code.

Desugaring rules:

- `try B catch (e) H` is syntactic sugar for `recover(\() -> do { B }, \(e) -> do { H })`.
- `try B finally F` is syntactic sugar for `do { v = recover(\() -> do { B }, \(e) -> do { F; fail(e) }); F; v }` (`v` is a fresh identifier that does not collide with others). `F` is evaluated exactly once on both the success and the failure path.
- `try B catch (e) H finally F` is equivalent to `try (try B catch (e) H) finally F` (`catch` is applied first, and `finally` is applied to the whole).

Typing rules:

- `recover` is typed as `Function<Function<T>, Function<Error, T>, T>`.
- `fail` is typed as `Function<Error, T>`. The return type `T` is instantiated from the context's expected type by built-in polymorphism (4.4).
- A `TryExpr` with `catch` is well-typed only when the types of the body block and the `catch` block are the same `T`, and the type of the whole expression is `T`.
- The `e` of `catch (e)` is in effect only inside the `catch` block, with type `Error`.
- The type of the `finally` block is arbitrary, and its value is discarded. The expression type of `try B finally F` is the type of the body block.

Catch rules:

- The targets of catching are runtime errors and external I/O errors that occur during evaluation of the body block. Syntax and static errors are detected before evaluation and are therefore not targets.
- A failure that occurs inside a `catch` block or a `finally` block is not caught by the same `TryExpr` and propagates outward.
- When `TryExpr`s are nested, the innermost `TryExpr` catches first.

Examples:

```lask
build(): String = do {
  out = try {
    $ make build
  } catch (e) {
    if (e.code == 2) {
      $ make clean
      $ make build
    } else {
      fail(e)
    }
  } finally {
    $ rm -rf ./tmp
  }
  out
}
```

## 7. Static Semantics

This chapter defines the semantics that can be verified before execution.

Static verification is performed in the following order.

1. Module dependency resolution (including cycle detection)
2. Top-level symbol table construction
3. Name resolution
4. Type annotation consistency and type inference
5. Call consistency checking

### 7.1 Verification Context

Static verification uses the following three kinds of environments.

- Module environment `M`: the set of import-resolved modules
- Type environment `Δ`: type aliases and type name resolution information
- Value environment `Γ`: type bindings of variables, functions, and parameters

Type judgments are written in the form `Γ ⊢ e : T`.

### 7.2 Name Resolution Order

Identifiers are resolved in the following order of precedence.

1. Innermost local bindings (lambda parameters, `do` bindings, `for` iteration variables)
2. Top-level declarations of the current module
3. Symbols brought in by named imports (under their renamed names)
4. `namespace.symbol` references of namespace imports
5. Symbols of the built-in library (Chapter 15) and the reserved identifier `stdin` (9.3)

Resolution rules:

- If multiple candidates exist at the same precedence level, it is an ambiguous reference error.
- If no candidate exists at any precedence level, it is an undefined reference error.
- Reserved words must not be bound as identifiers.
- Symbols of the built-in library are resolved at the lowest level (fifth precedence), so a user-defined symbol of the same name always takes priority (shadowing; 15.1).
- However, core function names specified as non-overridable (`spawn`, `choose`, `map`, `filter`, `reduce`, `forEach`, `runCommand`, `recover`, `fail`; Chapter 6) and `stdin` (9.3) must not be declared or bound at any of the first through fourth precedence levels. A violation is a duplicate definition error (`E-NAME-DUPLICATE`).

### 7.3 Scope and Shadowing

Scope rules:

- Lambda parameters are valid only within the lambda body.
- A `BindStmt` in a `do` is valid within the same block for the statements that follow the declaring statement.
- Bindings inside `if` / `for` / `try` blocks do not leak outward.
- The `x` in `for (x : xs)` is valid only within the iteration body.
- The `e` in `catch (e)` is valid only within the `catch` block.

Shadowing rules:

- Hiding a same-named binding of an outer scope from an inner scope is permitted.
- Rebinding the same name within the same scope is a duplicate definition error.
- At the top level, value declarations and function declarations of the same name must not be duplicated.

### 7.4 Integration of Type Annotations and Inference

The relationship between type annotations and inference is as follows.

- If an annotation is present, verification uses the annotated type as the expected type.
- If no annotation is present, the type is inferred by the rules of 4.3.
- If the inferred type does not conform (4.4) to the annotated type, it is a type mismatch error.

Supplementary rules:

- A `NamedType` is expanded to its `TypeAliasDecl` before checking.
- Because overloading is not permitted, one symbol has only one function type.
- `Any` is broadly accepted on the receiving side (any type conforms to `Any`), but the transition from `Any` to a concrete type can only be performed via a runtime type check with `cast` (15.8) (4.4).

### 7.5 Call Consistency

A call `f(a1, ..., an)` is typable only when `f` resolves to `Function<T1, ..., Tm, R>`.

Declaration parameter information:

- The parameters of a function declaration or lambda expression consist of positional parameters (all required; let their count be `m`), at most one variadic parameter, and a set of keyword parameters (name, type, and default-value expression) (6.1).
- Only positional parameters (and the array type of the variadic parameter) appear in the type argument sequence of the function type `Function<T1, ..., Tm, R>`. Parameter names, keyword parameters, and variadicity are not included in the function type; they are retained as static information attached to the declaration (declaration parameter information). Keyword binding, default-value completion, and variadic collection all depend on this information and are available only when the callee can be statically resolved to a declaration.

Verification items (argument binding):

- When `f` statically resolves to a function declaration or lambda expression, arguments are bound to parameters by the following procedure.
  1. Positional arguments are bound to the positional parameters in declaration order, from the front. If there are fewer than `m` positional arguments, it is `E-TYPE-ARITY`.
  2. Positional arguments beyond `m` are collected in written order into a single array and bound if a variadic parameter exists (variadic collection); otherwise it is `E-TYPE-ARITY`. If there are zero positional arguments corresponding to the variadic parameter, an empty array is bound.
  3. Keyword arguments are bound to keyword parameters whose names match. Unknown names, name-based specification of positional or variadic parameters, and duplicate bindings are `E-TYPE-KEYWORD` (6.1).
  4. Keyword parameters left unbound are completed at call time with their default-value expressions according to the rules of 8.3 (default-value completion).
- When `f` resolves only as a value of function type (application to a function-typed parameter, application of the result of evaluating an expression, values with only a `Function<...>` annotation, etc.), the declaration parameter information cannot be referenced, so only positional arguments are permitted and all positional parameters must be given explicitly (`n = m`). Use of keyword arguments is `E-TYPE-KEYWORD`, and all keyword parameters are completed with their defaults.

Verification items (types and result):

- Each bound argument must conform to the type `Ti` of its corresponding parameter position.
- The result type `R` must be passable to subsequent expressions.

Consistency of helper functions (normalization targets of syntactic sugar):

- `spawn`: `Function<Function<T>, AsyncHandle<T>>`
- `await`: `Function<AsyncHandle<T>, T>`
- `choose`: `Function<Bool, Function<T>, Function<T>, T>`
- `map`: `Function<Array<T>, Function<T, U>, Array<U>>`
- `filter`: `Function<Array<T>, Function<T, Bool>, Array<T>>`
- `reduce`: `Function<Array<T>, U, Function<U, T, U>, U>`
- `forEach`: `Function<Array<T>, Function<T, U>, Void>`
- `recover`: `Function<Function<T>, Function<Error, T>, T>`
- `fail`: `Function<Error, T>`
- `runCommand`: `Function<String, CommandResult>` (with a keyword parameter `--env: Environment = #local`)

Type variables in signatures (`T`, `U`, etc.) are instantiated and checked per call according to the built-in polymorphism rules of 4.4.

`async` / `if` / `for` / `$ ...` are statically verified as sugar over the helper functions above. `await e` is verified as an application of the core function `await`.

Consistency of environment expressions:

- The arguments of an environment expression `#kind(args)` (6.7) are subject to the same verification items as this section (argument binding, type conformance, default-value completion) against the environment constructor signatures of 10.2. Because environment constructor signatures always resolve statically as declarations, default-value completion and named arguments are always available.
- Violations concerning named arguments (unknown names, duplicate bindings) are reported as `E-TYPE-ENV-CONSTRUCT` for environment expressions (7.7).

### 7.6 Static Expansion Order of Syntactic Sugar

The expansion order during static verification is as follows.

1. `async e` -> `spawn(\() -> e)`
2. `await e` -> core function application `await(e)` (normalization of the parenthesis-omitted form)
3. Continuation-distribution transformation of `return` (including guard statements) (6.5)
4. `if (c) { ... } else { ... }` -> `choose(c, \() -> do { ... }, \() -> do { ... })`
5. `for (x : xs) { ... }` -> `map(xs, \(x) -> do { ... })` (or `forEach(xs, \(x) -> do { ... })` when the type of the body block is `Void`; 6.4)
6. `try ... catch (...) { ... }` / `finally { ... }` -> expansion to expressions using `recover` / `fail` (6.9)
7. Sequential-execution expansion of `do { ... }`
8. Expansion of environment expression sugar (`#name` -> `#name()`, and `#image-name` other than environment kind names -> `#docker("image-name")`)
9. Command sugar expansion of `$ cmd` / `$[env] cmd`

Type checking is performed on the core expressions after expansion. The meaning of the expansion result must be equivalent to that of the original syntax.

Only the choice of expansion target in step 5 (`map` / `forEach`) depends on the type of the body block (type-directed expansion). Implementations must type the body block first and determine the expansion target from that result. All other expansions are purely syntactic.

### 7.7 Static Errors

The error kinds reported by static verification include at least the following.

- `E-NAME-UNDEFINED`: undefined reference
- `E-NAME-AMBIGUOUS`: ambiguous reference
- `E-NAME-DUPLICATE`: duplicate definition
- `E-TYPE-MISMATCH`: type mismatch
- `E-TYPE-ARITY`: function argument count mismatch (shortage or excess of positional arguments; 7.5)
- `E-TYPE-KEYWORD`: invalid keyword argument (unknown name, name-based specification of positional or variadic parameters, duplicate binding, application to a function-typed value; 6.1, 7.5)
- `E-TYPE-CALL`: invalid call (calling a non-function value, etc.)
- `E-TYPE-COMMAND-ENV`: invalid command execution environment type
- `E-TYPE-ENV-CONSTRUCT`: invalid environment expression or environment definition (unknown environment kind, missing positional argument, unknown or duplicate named argument, non-literal argument to `env`, invalid environment definition file, or undefined environment name; 10.3)
- `E-TYPE-ACCESS`: invalid accessor (field access on a non-`Record`, unknown field, invalid index type)
- `E-TYPE-FIELD-DUPLICATE`: duplicate record field name or object literal key (4.2)
- `E-TYPE-ILLFORMED`: violation of type well-formedness rules (invalid position of `Void`, recursive type alias, invalid target type of `cast`; 4.2, 15.8)
- `E-MODULE-CYCLE`: module circular dependency
- `E-MODULE-UNRESOLVED`: unresolvable import (undeclared dependency name, or a declared dependency not present or not verified in the cache; Chapter 5)

Error diagnostics include at least the following.

- Error code
- Location information (line and column, or span)
- Expected type and actual type (where applicable)
- Resolution candidates (for ambiguous references)

## 8. Dynamic Semantics

This chapter defines the evaluation order of expressions and their runtime meaning.

Dynamic semantics is defined over the core expressions statically expanded in 7.6.
An evaluation result is either a "success value" or a "failure".

### 8.1 Evaluation Relation

The evaluation relation is written as follows.

- `⟨ρ, A, e⟩ ⇓ v`: under environment `ρ` and asynchronous state `A`, expression `e` evaluates to value `v`.
- `⟨ρ, A, e⟩ ⇑ err`: under environment `ρ` and asynchronous state `A`, expression `e` stops with failure `err`.

Here `ρ` is the value binding environment and `A` is the set of `AsyncHandle` states (running, success, failure).

Principles of evaluation order:

- Function arguments are evaluated from left to right.
- Array and record elements are also evaluated from left to right.
- Short-circuit operations (`&&`, `||`) follow the rules of 6.2.

### 8.2 Values and Closures

Runtime values include at least the following.

- Basic values (`Number`, `String`, `Bool`, `Null`)
- Environment values `Environment` (an environment kind and a normalized parameter set)
- Array values, map values, record values
- Function closures (a lambda body and its definition-time environment)
- Asynchronous handle values `AsyncHandle<T>`

Function values evaluate to closures, and free variables are bound in the definition-time environment.

### 8.3 Function Application

The evaluation procedure for `f(a1, ..., an)` is as follows.

1. Evaluate `f` and confirm that it is a function closure.
2. Evaluate the arguments from left to right in written order (positional arguments, followed by keyword arguments in written order).
3. Bind positional arguments to the positional parameters in declaration order, bind those exceeding the number of positional parameters as array elements of the variadic parameter (preserving evaluation order), and bind keyword arguments to keyword parameters whose names match (7.5).
4. The default-value expression of a keyword parameter is evaluated only when the corresponding argument is not given.
5. Evaluate the function body in the extended environment and return the result.

Default argument rules:

- Default-value expressions are evaluated at call time (not at definition time).
- The evaluation environment of a default-value expression includes the previously bound arguments.
- If the evaluation of a default-value expression fails, the entire function call fails.

### 8.4 Sequential Execution of `do`

`do { s1; s2; ...; sn }` is evaluated from top to bottom (the statement separator `;` is synonymous with a newline; see 6.5).

- A `BindStmt` evaluates the right-hand side, binds it to the identifier, and carries the environment forward to subsequent statements.
- An `ExprStmt` evaluates the expression and discards the value (except for the final statement).
- The result of the whole `do` is the evaluation result of the final statement. If it contains no statements, the result is `Void`.
- If an intermediate statement fails, subsequent statements are not evaluated and the whole `do` fails.

### 8.5 Control Structures (Core Functions)

Evaluation of `choose(c, t, f)`:

1. Evaluate `c`.
2. If `c = true`, evaluate only `t()`.
3. If `c = false`, evaluate only `f()`.
4. The unselected branch is not evaluated.

Evaluation of `map(xs, body)`:

1. Evaluate `xs` to obtain an array value.
2. Traverse the elements from left to right, evaluating `body(xi)` for each element `xi`.
3. Collect each result into an array in the same order and return it.
4. If any iteration fails, stop there and the whole expression fails.

Evaluation of `filter(xs, pred)`:

1. Evaluate `xs` to obtain an array value.
2. Traverse the elements from left to right, evaluating `pred(xi)` for each element `xi`.
3. Collect only the elements whose result is `true` into an array in the same order and return it.
4. If any predicate evaluation fails, stop there and the whole expression fails.

Evaluation of `reduce(xs, init, f)`:

1. Evaluate `xs` to obtain an array value.
2. Evaluate `init` as the initial accumulator value.
3. Traverse the elements from left to right, evaluating `f(acc, xi)` for the accumulator `acc` and each element `xi`, and take the result as the new accumulator value.
4. Return the accumulator value after traversal completes. If any application fails, stop there and the whole expression fails.

Evaluation of `forEach(xs, body)`:

1. Evaluate `xs` to obtain an array value.
2. Traverse the elements from left to right, evaluating `body(xi)` for each element `xi` and discarding the result.
3. Return `Void` after traversal completes.
4. If any iteration fails, stop there and the whole expression fails.

### 8.6 Asynchrony (Core Functions)

Evaluation of `spawn(thunk)`:

1. Evaluate `thunk` as a function value.
2. Create a new handle `h` and set `A[h] = running`.
3. Submit `thunk()` for asynchronous execution.
4. Return `h` to the caller immediately.

On asynchronous task completion:

- On normal completion, `A[h] = success(v)`.
- On failure, `A[h] = failure(err)`.

Evaluation of `await(h)`:

1. Evaluate `h`.
2. If `A[h] = running`, wait until completion.
3. If `A[h] = success(v)`, return `v`.
4. If `A[h] = failure(err)`, rethrow `err`.

Multiple `await`s on the same `h` return the same completion result.

### 8.7 Command Execution (Core Function)

Evaluation of `runCommand(cmd, env = ...)`:

1. Evaluate `cmd` to a string (for command sugar containing interpolation, the interpolation expressions are evaluated from left to right and the finalized string is passed).
2. Evaluate `env`. If unspecified, it is completed with the default value `#local` (7.5).
3. Resolve `env` to an execution environment (10.4).
4. Execute the command synchronously in the resolved environment. During execution, the child process's standard output and standard error are relayed to stderr in real time according to the rules of 12.3 (the relay does not affect the evaluation result).
5. After the command completes, construct and return a `CommandResult` value (6.6). It is a success regardless of the exit code.

Construction rules for `CommandResult`:

- `code`: set to the process's exit code as is. Termination by signal may be mapped according to shell convention (`128 + signal number`).
- `stdout` / `stderr`: set to the contents of each stream (up to an implementation-defined size limit; on overflow, a summary preserving the head and tail may be used).
- `runCommand` fails only in the case of execution infrastructure failures (environment unresolvable, connection failure, command unable to start, etc.) (external I/O error; Chapter 14).

Error value conversion of failures caused by non-zero exit (carrying over the exit code and standard error output) follows 8.10.

### 8.8 Evaluation of Environment Expressions (Core Expression)

An environment expression `#kind(args)` (6.7) is a core expression that remains after sugar expansion (7.6) and is evaluated by the following procedure.

1. Evaluate the argument expressions from left to right in written order (positional arguments, followed by named arguments).
2. Bind the evaluated values to the parameters of the environment constructor signature (10.2), and complete any keyword parameters not given by evaluating their default-value expressions (identical to the default argument rules of 8.3).
3. Return an `Environment` value consisting of the environment kind and the normalized parameter set.
4. If the evaluation of an argument expression or a default-value expression fails, the entire environment expression fails.

Resolution of an environment value to an execution environment (10.4) is performed not at environment expression evaluation time but immediately before command launch by `runCommand`.

Equality comparison:

- `==` / `!=` (6.2) on `Environment` values is determined by structural equality of the environment kind and the normalized parameter set.

### 8.9 Evaluation of Accessor Expressions (Core Expression)

Evaluation of field access `e.f`:

1. Evaluate `e` to obtain a record value.
2. Return the value of field `f`. The existence of the field is guaranteed by static verification (6.8), so no failure occurs at this stage.

Evaluation of index access `e[i]`:

1. Evaluate `e`.
2. Evaluate `i`.
3. If `e` is an array value: if `i` is a non-negative integer less than the number of elements, return the corresponding element. A non-integer, negative number, or out-of-range value is a runtime error (`E-RUNTIME-ACCESS`).
4. If `e` is a map value: if key `i` exists, return the corresponding value. If absent, it is a runtime error (`E-RUNTIME-ACCESS`). The failure contract is identical to that of `get(m, k)` (15.4).
5. If `e` is a record value: the index is restricted to a string literal, and the existence of the field is guaranteed by static verification (6.8), so the value of the corresponding field is returned. No failure occurs at this stage.

If the evaluation of a subexpression fails, the entire accessor expression fails.

### 8.10 Failure Propagation and Recovery

Failure propagation rules:

- If a subexpression fails, the failure propagates to the enclosing expression unless explicitly caught.
- `choose` propagates only the failure of the selected branch.
- `map` / `filter` / `reduce` / `forEach` return the first failure that occurs as the failure of the whole.
- `await` rethrows the corresponding asynchronous failure.
- `recover` catches failures of the body (described later in this section). Failures inside the handler are not caught and propagate.

Error value conversion rules:

When a failure is caught, and when it reaches the top level, the failure is mapped to an `Error` value (6.9) by the following rules.

- Runtime error: `code` is `2` (the same value as the exit code classification of 11.3).
- External I/O error: `code` is `3` (same as above).
- Failure via `fail(err)`: the given `err` is used as is. Failures arising from non-zero exits of the command execution expressions `$`, `$1`, and `$2` fall under this case; by the sugar expansion (6.6), `code` = the command's exit code and `message` = the contents of standard error output (signal mapping and capture limits follow the `CommandResult` construction rules of 8.7).
- Secret information contained in `message` is masked or removed according to the rules of 12.8.

Evaluation of `recover(body, handler)`:

1. Evaluate `body()`.
2. If it succeeds, return its value. `handler` is not evaluated.
3. If it fails, map the failure to an `Error` value `err`, evaluate `handler(err)`, and return its result.
4. Failures during the evaluation of `handler` are not caught by this `recover` and propagate outward.

Evaluation of `fail(err)`:

1. Evaluate `err` to obtain an `Error` value.
2. Raise a failure carrying that `Error` value.

Termination rules for uncaught failures:

- A failure that propagates to the top level (the outermost shell of the evaluation started by the CLI) is converted to an `Error` value and terminates the process. The process exit code is the `code` of the `Error` value (11.3, 14.8).
- When reflecting into the exit code, if `code` is not an integer between 1 and 255 (0, negative, non-integer, or greater than 255), it is normalized to `1`. The `Error` value itself is not modified.
- Error diagnostic output to stderr follows Chapter 14.

## 9. Standard I/O and Data Flow

This chapter defines standard input, standard output, standard error, and data passing between functions.

### 9.1 I/O Channel Model

The Lask runtime must distinguish and handle at least the following three channels.

- Standard input (stdin): input data received from the outside before execution starts
- Standard output (stdout): the channel for outputting evaluation results (`eval`; 9.5)
- Standard error (stderr): the channel for outputting diagnostic and failure information

Channel separation rules:

- Implementations must logically separate stdout and stderr.
- The main result must be output to stdout, and diagnostic information must be output to stderr.
- Child process output obtained from a command execution expression (6.6) is converted to a value and returned to expression evaluation according to the rules of 8.7, while simultaneously being relayed to stderr in real time as the command execution log (12.3).

### 9.2 Ingestion of Standard Input

Ingestion of standard input targets `run` / `eval` and is performed before function evaluation begins by the following procedure.
Because `repl` uses stdin as an interactive input channel, it does not perform the ingestion of this section, nor does it bind the `stdin` reference variable (9.3).

1. Read stdin until EOF.
2. Fix the entire input that was read as a single input snapshot.
3. Decode the input snapshot to a UTF-8 string by the rules of 9.4 and produce a `String` value.
4. Bind the conversion result to the reference variable of 9.3 before starting function evaluation.

Rules:

- Behavior in which additional stdin is received during execution and reflected into evaluation is outside the scope of this specification.
- Empty stdin is also treated as a success case and is converted to the empty string `""`.
- If an I/O failure occurs before EOF is obtained, it is a runtime error.

### 9.3 Standard Input Reference Variable

The runtime must provide a read-only reference variable `stdin` for accessing the decoded standard input.

Rules:

- `stdin` is treated as a reserved identifier and must not be rebound by user definitions.
- The binding of `stdin` is established exactly once per execution unit.
- The visibility of `stdin` takes effect from the top-level environment at the start of evaluation, and it is referenceable according to the ordinary name resolution rules (7.2).
- `stdin` is treated as an immutable value; destructive updates are not permitted.
- `stdin` cannot be used in `repl`. Because no binding is provided, a reference is an undefined reference error (`E-NAME-UNDEFINED`).

Type rules:

- The static type of `stdin` is fixed to `String`. It must not be changed by implementation or CLI settings.
- Structuring such as line splitting or JSON interpretation is performed within the language by applying functions of the built-in library (e.g., `split(stdin, "\n")` (15.3), `fromJson(stdin)` (15.8)). The transition from the result of `fromJson` (`Any`) to a concrete type uses `cast` (15.8) in combination.

### 9.4 Standard Input Decoding

Standard input is always decoded as a UTF-8 string and bound to `stdin` as a `String` value.

Rules:

- The input snapshot must be decoded as UTF-8.
- No CLI options are provided for selecting input structuring (line splitting, JSON interpretation, etc.). This is because if the decoding method depended on settings outside the language, the type of `stdin` and the meaning of the program would change per invocation. Structuring is performed explicitly within the language as in 9.3.

Failure rules:

- Input that cannot be decoded as UTF-8 is a runtime error.
- Decoding errors must be mapped to the external I/O error classification of Chapter 14 (`E-IO-DATA-DECODE`).

### 9.5 Role of Standard Output

Standard output is the main channel for passing evaluation results to the outside.

Rules:

- When `eval` completes function execution, the evaluation result is output to stdout in the specified encoding scheme (11.3).
- `run` does not output the evaluation result to stdout. The main purpose of `run` is task execution, and observation of results is done via execution logs and execution events (Chapter 12).
- If the evaluation result of `eval` is `Void`, the stdout output may be empty.
- Nothing other than the return value must be output to stdout. Diagnostics, logs, and relays of child process output are all output to stderr (9.6, 12.3).
- Details of the output encoding scheme follow 11.3 and Chapter 13.

### 9.6 Role of Standard Error

Standard error is the dedicated channel for execution failures, warnings, and diagnostics.

Rules:

- stderr is the destination for all logs. At least the following must be output to stderr.
  - Command execution logs (real-time relay of the child process's standard output and standard error; 12.3)
  - Execution logs (12.2)
  - Error diagnostics for syntax, static verification, and runtime failures (Chapter 14)
- The contents of stderr may include machine-readable error codes (Chapter 14) in addition to human-readable messages.
- The presence or absence of output to stderr must not affect the meaning of stdout (the main result).
- A command's standard error output is, in parallel with the relay to stderr (12.3), captured into the `stderr` of `CommandResult` (8.7) and into the `message` of the failure values of `$`, `$1`, and `$2` (6.6).

### 9.7 Inter-Function Data Flow and Pipes

Data flow in Lask is expressed by function application, return values, and the pipe operator (6.2).

Data passing rules:

- Value passing between functions is performed only through positional arguments, keyword arguments, and return values.
- `e |> f` is sugar for `f(e)`, passing the left-hand result to the right-hand function.
- `f >> g` denotes function composition and is treated at evaluation time as a single data flow that passes the result of `f` to `g`.
- `map(xs, body)` aggregates element-wise data flow into an array. When the result is not needed and the purpose is side effects, use `forEach(xs, body)`.

Rules for connecting to I/O:

- The only entry point from external input into internal expressions is the `stdin` binding.
- The exits from internal expressions to external output are the stdout output of the evaluation result in `eval` (9.5) and the stderr output for failures and diagnostics.
- Command execution expressions reconnect external process output to the expression data flow as `String` values.

Example:

```lask
normalize() = do {
  text = stdin
  text
    |> trim
    |> toLower
}

fanout(xs: Array<String>) =
  map(xs, \(x) -> concat("item:", x))
```

## 10. Execution Environments

This chapter defines the execution environments that can be specified when invoking commands, and their responsibilities.

### 10.1 The `Environment` Type and Environment Expressions

`Environment` is a built-in primitive type (4.1) representing the execution-target context that `runCommand` (6.6) receives as the keyword parameter `env`. `Environment` values are constructed only by evaluating an environment expression (6.7) `#environment-kind(arguments...)`.

Rules:

- `Environment` must not be overridden by user-defined type aliases or value declarations.
- `Environment` values must be resolvable to at least the following 3 families.
  - `#local` (local execution)
  - `#docker(...)` (Docker execution environment, including the sugar `#image-name`)
  - `#env(...)` (reference to a named environment; resolved to `remote` etc. defined in the environment definition file (10.3))
- The `env` argument of `runCommand` must be normalized to an `Environment` value at evaluation time.
- The default execution environment (the environment used by `$ cmd` / `runCommand`; 6.6, 8.7) is always `#local`. The default execution environment must not be changed by CLI options or other external configuration. Commands to be executed anywhere other than locally must always specify the environment explicitly with `$[env]` (or the `env` argument of `runCommand`).

Type conformance:

- The type and signature of `runCommand` follow the definition in 7.5.
- Passing a value other than `Environment` to the `env` argument is a type error.

### 10.2 Target Environment Profiles and Environment Constructor Signatures

The execution environment profiles specified by this specification are defined as pairs of an environment kind name and an environment constructor signature. The parameter rules of the signature (positional binding, default values, type annotations) are identical to those of function parameters (6.1), and the caller supplies positional and named arguments via an environment expression (6.7).

The environment kinds that can be used in environment expressions are the 3 kinds `local`, `docker`, and `env`. Only environments that are self-contained on the host running Lask (`local`, `docker`) may be written directly in code. Environments that span hosts (`remote`) impair the reusability of functions due to differences in connection targets, so they are defined as named environments in the environment definition file (10.3) and referenced via `env`.

- `local`: local process execution on the calling host
  - Signature: `local()` (no parameters)
  - Notation example: `#local` (sugar for `#local()`)
- `docker`: execution inside a container specified by a Docker image
  - Signature: `docker(image: String, ...)`. `image` is required and must not be an empty string.
  - Implementations may extend the signature with keyword parameters (e.g. `--memory: String = implementation default`, `--cpus: Number = implementation default`). Unknown parameter names must be static errors (7.7).
  - Notation examples: `#docker("alpine:3.12", memory="4g")`, sugar `#alpine:3.12`
- `remote`: remote host execution via SSH
  - Signature: `remote(host: String, --user: String = implementation default, --port: Number = 22)`. `host` is required, and the implementation default for `user` is the current user.
  - `remote` cannot be used in environment expressions. It can only be constructed as an entry in the environment definition file (10.3), and the signature is used to validate the entry's `params`.
- `env`: reference to a named environment defined in the environment definition file (10.3)
  - Signature: `env(name: String)`. `name` is required.
  - The actual argument for `name` must be a string literal containing no interpolation. Any other expression is a static error (`E-TYPE-ENV-CONSTRUCT`). This makes the set of named environments referenced by a module statically determinable.
  - Notation example: `#env("ansible")`

Each profile has at least the following execution attributes.

- Process launch method
- Working directory resolution rules
- Environment variable injection rules
- Permission boundary and access constraints

Implementations may extend with additional profiles (pairs of environment kind and signature), but must not break compatible behavior with the above 3 profiles.

### 10.3 Environment Definition File

Connection information for execution environments that span hosts (`remote`) is not written in Lask code; it is defined as named environments in the environment definition file. Code references only the name via the environment kind `env` (10.2). This ensures that differences in connection targets do not affect the reusability of task functions.

File resolution:

- The default file name is `environments.lask.json`, loaded from the base directory for module resolution (the same location as `main.lask`).
- The file to load can be replaced with the CLI's `--env-file <path>` (11.1). Replacement is a full substitution at the file level; merging of multiple files is not performed.
- When external dependencies (Chapter 5) are used, only the root project's environment definition file is in effect. Environment definition files carried by external modules have no effect.

Schema:

```json
{
  "environments": {
    "ansible": {"kind": "remote", "params": {"host": "203.0.113.10", "user": "automation"}},
    "builder": {"kind": "docker", "params": {"image": "golang:1.22"}}
  }
}
```

- The top-level `environments` is a map from environment names to entries. Environment names must be strings conforming to `lower_id` (3.2).
- Each entry has a `kind` (environment kind name) and `params` (parameter set). This is the same form as the `Environment` metadata representation in 13.1.
- Only concrete kinds (`local`, `docker`, `remote`) may be specified for `kind`. Specifying `env` (chained indirect references) is a static error.
- Each key of `params` binds by parameter name (because this is a data representation, positional parameters are also given by name). Satisfaction of positional parameters, prohibition of unknown key names, and default-value completion of keyword parameters are checked in accordance with 7.5.
- Credentials (passwords, private keys) must not be written in this file. Credentials are supplied from execution settings or a credential store, following the rules in 10.9.

Static validation:

- If a module contains one or more `#env(...)`, the environment definition file becomes an input to static validation (Chapter 7). Absence of the file, malformed syntax, schema violations, signature nonconformance of entries, and undefined referenced names are all static errors (`E-TYPE-ENV-CONSTRUCT`).
- Because the argument of `#env` is limited to a string literal (10.2), the set of named environments referenced by a module is statically determined. All environments to be used can be enumerated and validated before execution (`envs` in 11.4).
- For modules that do not contain `#env(...)`, the environment definition file is unnecessary.

Example:

```lask
provision(): String =
  $[#env("ansible")] ansible-playbook site.yml
```

### 10.4 Environment Resolution Rules

Environment resolution is performed immediately before command launch by `runCommand`.

Resolution procedure:

1. Evaluate the `env` expression to obtain an `Environment` value (environment kind and normalized parameters).
2. Classify the value's environment kind. `env` is replaced with the entry from the environment definition file (per the kind determination rules) and then classified as one of `local` / `docker` / `remote`.
3. Check satisfaction of positional parameters (`image` for `docker`, `host` for `remote`) and value constraints (non-empty `image`, etc.).
4. Normalize into an executable concrete runtime configuration.

Kind determination rules:

- The environment kind name of the environment expression becomes the environment kind of the `Environment` value as is (`#local()` is `local`, `#docker(...)` is `docker`, `#env(...)` is `env`).
- Values of environment kind `env` are resolved by substituting the kind and parameters of the corresponding entry in the environment definition file (10.3). Existence of the name and validity of the entry have already been statically validated (10.3). The subsequent rules are applied to the kind after substitution.
- The sugar `#image-name` has already been expanded to `#docker("image-name")` by static expansion (7.6).
- An unknown environment kind is a static error (`E-TYPE-ENV-CONSTRUCT`) and must not reach runtime environment resolution.

Failure rules:

- Unknown kinds and missing parameters are static errors (`E-TYPE-ENV-CONSTRUCT`) and do not occur in the runtime resolution of this section. The only means of specifying an execution environment is the in-language environment expression (no means of supplying an execution environment from the CLI is provided; 11.1).
- Violation of parameter value constraints (empty image name, etc.) or absence of the execution infrastructure (Docker daemon absent, SSH connection impossible, etc.) is a pre-execution error.
- Resolution failure must be classified as an external I/O or runtime error in Chapter 14.

Determinism:

- For the same `Environment` value, the same environment definition file content, and the same execution settings, the environment resolution result must be deterministic.

### 10.5 Working Directory Rules

The working directory (cwd) is the base for relative path resolution during command execution.

Rules:

- The default cwd for `local` is the project base directory where execution started.
- The default cwd for `docker` is the working directory determined by the implementation inside the container.
- The default cwd for `remote` is the user's initial directory resolved at the connection target, or an implementation-configured value.
- An explicit specification takes precedence over the default cwd.
- If the cwd does not exist or is inaccessible, it is a pre-execution error.

Portability requirements:

- Implementations must absorb differences in path separators and absolute/relative resolution so that the intent of the same script is preserved.

### 10.6 Environment Variable Rules

The execution environment constructs the environment variable set `E` at process launch.

Construction rules:

1. Obtain the execution environment's default variables as the base set.
2. Apply override variables from implementation settings or CLI specification.
3. Exclude variables prohibited by security policy.

Conflict rules:

- If the same key is supplied by multiple sources, resolve with the precedence: explicit specification > implementation settings > environment defaults.

Compatibility rules:

- Case sensitivity of variable names is execution-infrastructure dependent, but must be handled with consistent rules within the same infrastructure.

### 10.7 Permission Boundary

Each profile must explicitly have the following permission boundaries.

- Filesystem access boundary
- Network access boundary
- Executable command boundary

Rules:

- `local` is bounded above by the calling process's privileges.
- `docker` is bounded above by the privileges within the container isolation boundary.
- `remote` is bounded above by the privileges of the principal (user/role) at the connection target.
- If execution is impossible due to insufficient privileges, it is a runtime error, and the cause must be reported in a diagnosable form.

Security requirements:

- Implementations must not output credentials or secret values in plaintext to logs.
- Implementations may record command execution requests with the minimum auditable information (environment kind, target, exit status).

### 10.8 Responsibilities for Absorbing Environment Differences

In this specification, absorbing environment differences refers to the responsibility of making the same Lask program deployable to different execution infrastructures while preserving its meaning.

Responsibilities:

- Unify the return value and failure contract of the command execution API (`runCommand`) across environments.
- Standardize the observation interface for exit codes, stdout, and stderr.
- Map environment-specific failures to the common error classification of this specification (Chapter 14).
- To the extent possible, internalize implementation differences such as retries and connection initialization, and do not leak them into language-level semantics.

Non-responsibilities:

- Complete absorption of shell dialect differences themselves is not required.
- Semantic differences of external tools specified by the user (behavioral differences of the commands themselves) are outside the responsibility of this specification.

Example:

```lask
showLocal() = $[#local] pwd

showDocker() =
  $[#alpine:3.20] pwd

showRemote() =
  $[#env("ansible")] pwd
```

### 10.9 The SSH Execution Model for `remote`

The `remote` environment connects to a server via SSH and executes commands. It is constructed only as an entry in the environment definition file (10.3) (`kind: "remote"`), and is referenced from code via `#env(name)` (10.2).

Forms of the entry's `params` (corresponding to the `remote` signature in 10.2):

- `{"host": "..."}`
- `{"host": "...", "user": "..."}`
- `{"host": "...", "user": "...", "port": ...}`

Here `host` and `user` are strings, and `port` is a number.
Unspecified values use the defaults of the signature in 10.2 (current user, port 22).

Execution procedure:

1. Normalize the resolved `remote` parameters (10.4) into SSH connection parameters.
2. Apply the host key verification policy and determine whether the connection is permitted.
3. Establish the SSH session.
4. Apply the cwd rules of 10.5 and the environment variable rules of 10.6 to the connection target.
5. Execute the specified command in non-interactive mode.
6. Collect the exit code, stdout, and stderr, and map them to the `runCommand` contract of 8.7.

Failure rules:

- DNS resolution failure, connection refusal, authentication failure, host key mismatch, and timeout are runtime errors.
- Non-zero command exit after SSH channel establishment is treated as an ordinary command failure.

Security rules:

- Implementations must enable host key verification by default.
- Passwords and private key bodies must not be output to error messages or audit logs.
- Credentials should be supplied from execution settings or a secure credential store, and it is preferable not to hold them in plaintext as expression values.

Operational rules:

- Implementations may reuse SSH connections (ControlMaster, etc.) as necessary.
- Whether connections are reused must not affect the semantics, and the observable results (stdout/stderr/exit code) must be equivalent.

## 11. CLI Specification

This chapter defines the external contract of the CLI.

### 11.1 Subcommands

The CLI must provide the following subcommands.

- `serve`: provides editor integration (Language Server Protocol) functionality.
- `check`: returns static validation results.
- `run`: executes the specified function. The evaluation result is not output to stdout (11.3).
- `eval`: executes the specified function and outputs the evaluation result to stdout. The syntax is identical to `run` (11.2).
- `infer`: returns type inference results.
- `repl`: provides an execution environment for interactively evaluating expressions and functions.
- `envs`: enumerates the execution environments used by tasks and checks their accessibility (11.4).
- `deps`: manages external dependencies — fetches and verifies them, and records new entries (11.5).

Basic invocation syntax:

```text
lask <subcommand> [options]
```

Common options:

- `--module <path>`: specifies the target module for execution. When unspecified, `main.lask` in the execution directory is used.
- `--format <text|json>`: specifies the display format of CLI responses.
- `--trace-id <id>`: specifies the execution trace identifier.
- `--no-color`: disables decorative colors in the output.

Minimal syntax per subcommand:

```text
lask serve [--stdio | --tcp --port <n>]
lask check [--module <path>]
lask run [--module <path>] <function> [args ...]
lask eval [--module <path>] <function> [args ...]
lask infer [--module <path>] [--symbol <name>]
lask repl [--module <path>]
lask envs [--module <path>] [--env-file <path>] [<function>] [--check]
lask deps sync [--module <path>]
lask deps add <name> (--git <url> --rev <rev> | --url <url>) [--module <path>]
```

Policy on environment specification:

- Specification of the execution environment is performed only via in-language environment expressions (6.7). No option (`--env` etc.) is provided for specifying or overriding the execution environment from the CLI.
- Consequently, all execution environment specifications pass static validation (7.5, 7.7), and unknown environment kinds never reach runtime environment resolution (10.4).
- To switch the execution environment per invocation, receive it via a function parameter (a serializable type such as `String`) and construct the environment expression inside the function (e.g. `build(image: String) = $[#docker(image)] ...`).
- The default execution environment is always `#local` (10.1).
- Definitions of named environments (`env` in 10.2) are loaded from the environment definition file (10.3) specified with `--env-file <path>`. When unspecified, `environments.lask.json` in the base directory for module resolution is used. Replacement is a full substitution at the file level; merging of multiple files is not performed.

SSH execution settings options:

- `--ssh-known-hosts <path>`: specifies the known hosts file.
- `--ssh-strict-host-key-checking <yes|accept-new|no>`: specifies the host key verification policy.
- `--ssh-connect-timeout <sec>`: specifies the SSH connection timeout in seconds.

Rules:

- The `--ssh-*` options are connection settings and apply when resolution of a `remote` environment (10.9) occurs during execution, and to the connection checks of `envs --check` (11.4).
- If no `remote` environment is ever resolved, `--ssh-*` specifications may be ignored, but must not be treated as errors.
- Connection settings are not part of an `Environment` value and are treated as the "execution settings" referred to in 10.9. They must not be included in expression values or execution events.

### 11.2 Function Invocation

- The CLI calls functions directly. The function to call is specified as the first positional argument of `run` / `eval` (no `--function` option is provided).
- Function arguments are given as positional arguments and as keyword arguments in option form following common CLI conventions.
- The interpretation and decoding mode of arguments can be specified.

Function invocation syntax:

```text
lask run  [--module <path>] [--arg-decode <mode>] [lask options ...] <function> [args ...]
lask eval [--module <path>] [--arg-decode <mode>] [lask options ...] <function> [args ...]
```

- The invocation syntax of `run` and `eval` is identical. The rules of this section (function-name mapping, keyword arguments, binding, decoding) apply equally to both.
- When `--module` is unspecified, `main.lask` is the target. Therefore the minimal form is `lask run <function> [args ...]` / `lask eval <function> [args ...]`.
- Options of `lask` itself (`--module`, `--arg-decode`, `--ssh-*`, `--format`, etc.) must be placed before the function name. **All tokens after the function name are interpreted as arguments to the function** (a boundary rule to prevent collisions between `lask` options and function keyword arguments).

Function-name and parameter-name mapping rules:

- All occurrences of `-` in function names and keyword argument names are replaced with `_` before name resolution (allowing kebab-case invocation). Example: `lask run show-version` resolves the top-level function `show_version`, and `--out-dir dist` binds to the parameter `out_dir`.
- Matching is performed with the name after replacement; if there is no match, it is an undefined reference error.
- Since identifiers cannot contain `-` (3.2), this mapping never maps multiple candidates to the same name, and resolution is always unique.
- camelCase names (`buildAndTest` etc.) are not subject to the mapping and are specified with their names as is.

Keyword argument forms:

- A keyword argument for parameter `p` is given as `--p <value>` or `--p=<value>`.
- If the parameter name is a single character, the short form `-p <value>` may also be used.
- Example: for `retry(cmd: String, --n: Number = 3)`, `lask run retry "make test" -n 5` corresponds to the function call `retry("make test", n = 5)`.

Binding rules:

1. Resolve the target module (`--module`, default `main.lask`) and obtain the top-level function specified by the first positional argument.
2. Bind the positional arguments after the function name, from left to right, to the positional parameters in declaration order. Positional arguments exceeding the number of positional parameters are collected into the variadic parameter (variadic collection in 7.5).
3. Bind keyword arguments to parameters with matching names. Unknown names and duplicate bindings are pre-execution errors (corresponding to `E-TYPE-KEYWORD` in 7.5).
4. Keyword parameters that were not bound are completed with the default values from the function definition (default-value completion in 7.5).
5. If the number of positional arguments does not match the number of positional parameters (excluding collection by a variadic parameter), it is a pre-execution error.

Argument decoding modes:

- `--arg-decode text`: treats all arguments as `String`.
- `--arg-decode json`: interprets each argument as a JSON value.
- `--arg-decode auto`: JSON if interpretable as JSON, otherwise `String`.
- The default when unspecified is `auto`.
- Decoding applies to the values of both positional and keyword arguments.

Type conformance rules:

- After binding, each argument must satisfy the call conformance of 7.5.
- In `auto` mode, when ambiguous, `String` takes precedence.
- Decoding failure or type mismatch must be reported as an error before function evaluation begins.
- Functions with positional parameters of type `Environment` are excluded from direct CLI invocation (since no decoding mode can construct an `Environment` value, this is a pre-execution error). Keyword parameters of type `Environment` are completed with their default values, but values cannot be supplied from the CLI. To select the environment externally, receive it as `String` etc. and construct the environment expression inside the function.

Difference between `run` and `eval`:

- The only difference between `run` and `eval` is the handling of the evaluation result. `eval` outputs the evaluation result to stdout, and `run` does not (11.3).
- The semantics of binding, evaluation, failure propagation, and exit codes are identical for both.
- `eval` is intended for passing return values via shell pipes to other commands or another `lask` invocation, composing tasks ad hoc. For one-off task execution, the output of return values is noise, so `run` is normally used.

### 11.3 Input/Output Contract

The CLI's input/output follows the contract of Chapter 9 and must satisfy the following.

Standard input contract:

- stdin is always read as a UTF-8 string and bound to `stdin` as a `String` value (9.2–9.4). No option to select a decoding mode (`--stdin-decode` etc.) is provided.
- Structuring such as line splitting or JSON interpretation is performed inside the function by applying `split` / `fromJson` etc.
- `run` / `eval` accept stdin.
- `repl` uses stdin for interactive input, and therefore does not provide the `stdin` reference variable (9.2, 9.3).
- `check` / `infer` / `serve` may accept stdin, but the meaning must be defined in the subcommand specification.

Standard output contract:

- `--stdout-encode <text|json|pretty-json>` may be specified.
- The default when unspecified is `json`.
- `text` is human-readable display, `json` is machine-readable display, and `pretty-json` is formatted JSON display.
- `eval` outputs the evaluation result to stdout with the specified encoding.
- `run` does not output the evaluation result to stdout. `run` must not output anything to stdout.
- `check` and `infer` output diagnostic results to stdout.
- For both `run` / `eval`, the output destination for execution logs and diagnostics is stderr (9.6, Chapter 12).

Standard error contract:

- stderr is the output destination for logs (9.6). During execution of `run` / `eval`, the command execution log (12.3) is output to stderr in real time.
- On execution failure, the error code, a summary, and, as necessary, position information are output to stderr.
- When `--format json` is specified, stderr output uses JSON Lines as the canonical form (12.2).
- Secrets must not be output in plaintext to stderr.

Exit code contract:

- `0`: success
- Uncaught failure: the `code` of the `Error` value (8.10). Command failure passes through that command's exit code; other runtime errors default to `2`; external I/O errors (including SSH connection failure and environment resolution failure) default to `3`; `fail` passes through the specified value. If `code` is not an integer in 1–255, it is normalized to `1`.
- `1`: syntax or static validation error (detected before evaluation; no `Error` value is generated)
- `4`: CLI usage error (invalid option, missing argument, etc.)

Overlap of exit codes:

- If a command fails with exit code `1` or `4`, it cannot be distinguished from a syntax error or CLI usage error by the process exit code alone. Exit codes are primary information for recovery and script integration; accurate discrimination of the kind is performed with the machine-readable diagnostics on stderr (error codes; Chapter 14).

Representative examples:

```text
# Type check only (main.lask is the default target)
lask check

# Function execution (local; JSON is interpreted via fromJson(stdin) inside main.lask)
echo '{"name":"alice"}' | lask run greet

# Execution with keyword arguments (in main.lask: greet(name: String, --prefix: String = "hello"))
lask run greet alice --prefix hi

# Variadic arguments (in main.lask: sum(...xs: Array<Number>))
lask run sum 1 2 3

# Execution on a fixed image (in ci.lask: build() = $[#alpine:3.20] ...)
lask run --module ci.lask build

# Execution in a remote environment (in ops.lask: provision() = $[#env("ansible")] ...; environment definitions in environments.lask.json)
lask run --module ops.lask \
  --ssh-known-hosts ~/.ssh/known_hosts \
  --ssh-strict-host-key-checking yes \
  --ssh-connect-timeout 10 \
  provision

# Enumeration of used environments and access checks
lask envs provision --check
```

### 11.4 Environment Check (`envs`)

`envs` enumerates the execution environments used by tasks and checks their accessibility.

Syntax:

```text
lask envs [--module <path>] [--env-file <path>] [<function>] [--check]
```

Enumeration rules:

- If no function is specified, the environments referenced by the entire module are enumerated (static scan of environment expressions, matched against the definitions in the environment definition file (10.3)).
- If `<function>` is specified, the enumeration is limited to the environments used on the call graph reachable from that function. Function-name mapping follows 11.2.
- Reachability is an over-approximation. A superset of the environments that may be used must be reported; under-reporting is not permitted. Functions passed around as function values are included conservatively.
- If the `image` of `#docker(image)` is a runtime value, it is enumerated as a `docker` environment with a dynamic image. `#env` names are statically determined (10.2), so they are always enumerated concretely.

Check rules (`--check`):

- For each enumerated environment, only reachability is checked without executing any commands. The check must not have side effects on the target environment.
- `local`: always succeeds. The check may include an existence check of the default cwd (10.5).
- `docker`: checks connectivity to the Docker daemon. The depth of image reference resolution (local existence, registry query) is implementation-defined, and image retrieval (pull) is not performed.
- `remote`: checks up to establishment of the SSH connection (including host key verification and authentication). The security rules of 10.9 apply, and the `--ssh-*` options (11.1) may be used.
- The check does not abort on the failure of a single environment; all environments are checked and reported together.

Output and exit codes:

- For each environment, the reference name (for named environments), the environment kind, a summary of the target, and the check result (with `--check`) are output. Structured output is available with `--format json`.
- For environments that failed the check, an error code (`E-IO-SSH-CONNECT`, `E-IO-ENV-RESOLVE`, etc.; Chapter 14) and a summary are included.
- Exit codes: `0` if enumeration and checks all succeed; `3` if one or more environments are inaccessible; `1` for static errors (undefined environment names, invalid environment definition file, etc.); `4` for CLI usage errors (nonexistent function name, etc.) (following the classification in 11.3).

Execution example:

```text
$ lask envs provision --check
ansible  remote  automation@203.0.113.10:22  ok
builder  docker  golang:1.22                 ok
```

### 11.5 Dependency Management (`deps`)

`deps` manages the external dependencies declared in the dependency definition file (Chapter 5).

Syntax:

```text
lask deps sync [--module <path>]
lask deps add <name> (--git <url> --rev <rev> | --url <url>) [--module <path>]
```

Rules (`sync`):

- `deps sync` fetches all dependencies declared in `dependencies.lask.json` — including transitive dependencies of external modules — into an implementation-defined cache, and verifies each against its `hash`.
- `deps sync` is the only subcommand permitted to access the network for module resolution. All other subcommands resolve modules exclusively from the cache (Chapter 5).
- Fetched sources are stored content-addressed; re-running `deps sync` with an unchanged definition file performs no network access for already-verified entries.
- A hash verification failure is reported as `E-MODULE-HASH-MISMATCH` and the entry must not be placed in the cache.

Rules (`add`):

- `deps add` fetches the specified source, computes its content hash, records the entry under `<name>` in `dependencies.lask.json` (creating the file if it does not exist), and places the verified source in the cache.
- `<name>` must conform to `lower_id` (3.2). If an entry with the same name already exists, it is replaced (its source and hash are overwritten).
- The recorded hash follows the trust-on-first-use model: the content trusted at recording time is pinned, and any subsequent change to the published source is detected by `deps sync` as `E-MODULE-HASH-MISMATCH`.
- `--git` requires `--rev` (a tag or commit). `--url` accepts an archive or a single `.lask` file (Chapter 5).

Exit codes (common to `sync` and `add`):

- `0`: all dependencies fetched and verified.
- `1`: the dependency definition file is missing (while dependencies are declared), malformed, or violates the schema (Chapter 5).
- `3`: a fetch or verification failure (network failure, `E-MODULE-HASH-MISMATCH`).
- `4`: CLI usage error.

## 12. Observability

This chapter defines the means of observing execution.

### 12.1 Observation Targets and Design Principles

Observability is the requirement to provide, in a consistent format, the information needed for understanding execution, failure analysis, and operational auditing.

Implementations must treat at least the following as observation targets.

- Function call boundaries (start, end, failure)
- Environment execution boundaries (`#local` / `#docker(...)` / `#env(...)`)
- External command execution (start, exit code, output summary)
- Type validation and runtime diagnostics

Design principles:

- Low intrusiveness: the presence or absence of observation must not change the language semantics (Chapters 7, 8).
- Correlatable: logs and events belonging to the same execution must be traceable.
- Machine-readable: at least one structured format (JSON etc.) must be provided.
- Secret protection: sensitive information must not be exposed by default.

### 12.2 Execution Log

The execution log is time-series diagnostic information and must contain at least the following fields.

- `timestamp`: time (UTC recommended)
- `level`: `debug` / `info` / `warn` / `error`
- `traceId`: the identifier defined in 12.5
- `message`: human-readable message
- `context`: arbitrary auxiliary information (function name, module name, environment kind, etc.)

Output rules:

- `info` and below are classified as operational logs, and `warn` / `error` as diagnostic logs.
- `error`-level logs must be cross-referenceable with the corresponding failure event (12.6).
- The line format of the default text display is implementation-defined. However, each line contains `level` and `message`, and the command execution log (12.3) follows the text-format provisions.
- When `--format json` is specified, logs, command execution logs, error diagnostics, and execution events output to stderr must be output as JSON Lines (one object per line) (canonical form). The line kind can be discriminated by the presence of fields (`kind` = execution event (13.3), `code` + `stage` = error diagnostics (14.3), `stream` / `event` = command execution log (12.3)).

### 12.3 Command Execution Log

During execution of `runCommand` (8.7), the implementation must relay the child process's standard output and standard error to its own stderr in real time as the command execution log. The relay is performed in parallel with the capture into `CommandResult` (8.7) and must not affect the evaluation result (value semantics).

Relay rules:

- The relay is performed sequentially line by line (line buffering). Output must not be accumulated until the child process exits.
- The targets are `run` / `eval` execution and the connection diagnostics of `envs --check` (11.4). The relayed content is identical regardless of which sugar (`$`, `$1`, `$2`, `$*`) was used for execution.
- For each command execution, a 1-based sequence number (execution number) that is unique within the top-level execution is assigned. Execution numbers are not duplicated even under concurrent execution (`async`).
- Each log line contains a timestamp and an environment summary with the execution number. The executed command is recorded only on the start line; subsequent lines are correlated by the execution number (the command is not recorded on every line).
- The secret masking of 12.8 applies to relayed content. Output volume limits and suppression measures (rate limiting, suppression options, etc.) may be provided as implementation-defined.

Text format (default):

Each line is output with the following structure.

```text
<timestamp> [<environment summary>:<execution number>] <kind> <content>
```

- `timestamp`: UTC ISO 8601 format.
- Environment summary: follows the notation of environment expressions (6.7). `#local`, `#<image>` (docker), `#env(<name>)` (named environment).
- Execution number: the sequence number assigned by the relay rules.
- Kind: `$` (start line; records the executed command string as the content), `1|` (child process's standard output), `2|` (child process's standard error), `exit <code>` (exit, with exit code). `1` and `2` are file descriptor numbers (the same scheme as the stream specifiers in 6.6).
- The start line (`$`) and the `exit` line must be emitted for every command execution. The command string on the start line may be truncated to an implementation-defined length.

Output example:

```text
2026-07-23T10:15:04.123Z [#golang:1.22:1] $ go build ./...
2026-07-23T10:15:04.320Z [#golang:1.22:1] 1| compiling module...
2026-07-23T10:15:04.751Z [#golang:1.22:1] 2| warning: unused variable
2026-07-23T10:15:05.002Z [#golang:1.22:1] exit 0
```

JSON format (with `--format json`):

- Each line of the command execution log is output as one object of the JSON Lines of 12.2.
- Required fields for all lines: `timestamp`, `level`, `traceId`, `exec` (execution number).
- Start line: in addition to `event: "start"`, `command` (the executed command string) and `env` (the metadata representation of 13.1; named environments include `name`) are required.
- Relay lines: `stream` (`1` or `2`) and `message` (the line's content) are required. `command` and `env` may be omitted (correlated with the start line via `exec`).
- Exit line: `event: "exit"` and `code` (exit code) are required.

Level rules:

- Relay lines (`1|`, `2|`) and the start line (`$`) are `info`.
- `exit` is `info` when the exit code is `0`, and `warn` when non-zero.

### 12.4 Stack Traces

Implementations must be able to generate and record stack traces at all times upon execution failure. Generated stack traces must satisfy the following.

- Contain at least function names, call order, and, where possible, source positions (line/column or span).
- When crossing asynchronous boundaries (`spawn` / `await`), the parent-child relationship must be traceable.
- Failures involving command execution (`runCommand`) may include the environment kind and a summary of the executed command.

Omission rules:

- Internal runtime frames may be omitted in user-facing display.
- However, even with omission, the frames necessary to identify the cause of the failure must be retained.

### 12.5 Trace Identifier

The trace identifier `traceId` is an identifier for correlating observation information of the same execution unit.

Generation and propagation rules:

- When the CLI's `--trace-id` is specified, that value is adopted.
- When unspecified, the implementation automatically generates an identifier unlikely to collide.
- Logs, events, and errors belonging to the same top-level execution share the same `traceId`.
- During execution in a `remote` environment, the `traceId` may be propagated to the connection target as necessary.

Consistency rule:

- Multiple top-level executions must not be mixed under a single `traceId`.

### 12.6 Execution Event Output

Execution events are represented in the `ExecutionEvent` format of 13.3.

Event emission rules:

- Emit a `CallEvent` when a function call starts.
- Emit a `ReturnEvent` on normal termination.
- Emit a `FailEvent` on failure termination.

Minimum guarantees:

- Each `CallEvent` must ultimately correspond to exactly one `ReturnEvent` or `FailEvent`.
- Event order must preserve causal order for the same function execution.
- When command execution (`runCommand`) is involved, the `ArgumentsSummary` may include an environment kind summary.
- Even when a failure is caught by `try` / `catch` (6.9), the `FailEvent` corresponding to the failed function call is emitted. Catching must not cancel the event.

### 12.7 In-flight Diagnostics

Implementations may provide progress diagnostics for long-running executions or interactive modes.

Targets:

- `serve`: analysis start/completion, error counts, re-analysis triggers
- `repl`: evaluation start/end, summary of the most recent error
- `run` / `eval`: execution stages (resolution, type validation, evaluation, external command execution)

Rules:

- In-flight diagnostics must not corrupt the primary result (the stdout output of `eval`).
- Human-oriented progress display must be output to stderr or a dedicated channel.
- When `--format json` is specified, progress diagnostics may be output as structured events.

### 12.8 Protection of Sensitive Information and Retention Policy

Because sensitive information may be mixed into observation data, implementations must satisfy the following.

- Environment variable values, credentials, private keys, and tokens are masked or removed.
- Command arguments are summarized as necessary, avoiding full-text output.
- Even on SSH authentication failure, the secrets themselves must not be output.

Retention policy:

- The implementation shall define default retention periods and size limits.
- Rotation must not break event consistency (12.6).
- Where audit requirements exist, a tamper-evident storage format may be adopted.

## 13. Serialization Conventions

This chapter defines the external representation conventions for data values, function values, and events.

### 13.1 Data Values

This section defines the canonical serialization format for data values.

Format profiles:

- `json`: machine-readable canonical JSON format
- `pretty-json`: a pretty-printed format carrying the same information as `json`
- `text`: human-readable format (implementation-defined). However, it must not cause information loss

For `json` / `pretty-json`, the following mapping rules must be satisfied.

- `Number`: maps to a JSON number.
- `String`: maps to a JSON string.
- `Bool`: maps to a JSON boolean.
- `Null`: maps to JSON null.
- `Void`: not directly serializable (4.5). When output is required, convert to the tagged metadata `{"$type":"Void"}` (treated equivalently to `FunctionRef` in 13.2).
- `Environment`: not directly serializable (4.5). When output is required, convert to the tagged metadata `{"$type":"Environment","kind":"<environment kind>","params":{...}}`. `kind` is the environment kind name, and `params` is the normalized parameter set (e.g., `{"$type":"Environment","kind":"docker","params":{"image":"alpine:3.20"}}`). Parameters containing secret information are masked according to the rules in Chapter 12.
- `Array<T>`: maps to a JSON array preserving element order.
- `Map<T>`: maps to a JSON object with string keys.
- `Record<...>`: maps to a JSON object with field names as keys.

Rules for `Any`:

- `Any` maps according to the serialization rules of the underlying value.
- If the underlying value is `Void` or `Environment`, convert it to the tagged metadata representation; if the underlying value is a function value, convert it to a `FunctionRef` (13.2).

Round-trip rules for tagged metadata:

- `fromJson` / `decode` (15.8) do not treat the `$type` field specially, and read tagged metadata as an ordinary object (`Record` or `Map`).
- Therefore, restoring `Void`, `Environment`, or function values from their serialized results (round-trip conversion) is not guaranteed.

Numeric rules:

- `NaN`, `+Infinity`, and `-Infinity` cannot be represented directly in canonical JSON, so by default they are a serialization error.
- If an implementation provides an extended representation, it must be explicitly enabled via a CLI option.

Key and ordering rules:

- `Record<...>` may be output in definition order.
- `Map<T>` does not guarantee ordering.
- Duplicate identical keys are not permitted.

Character encoding:

- Strings must be input and output as UTF-8.
- Control characters and quotation marks must be escaped according to JSON rules.

### 13.2 Function Values

`Function<T1, T2, ..., R>` is not directly serializable.

Rules:

- Function bodies (code) and closure environments must not be externalized as-is.
- When a function value needs to be output externally, it must be converted to the reference metadata `FunctionRef`.

Minimum fields of `FunctionRef`:

- `$type`: fixed value `"FunctionRef"`
- `module`: defining module identifier
- `name`: function name
- `arity`: number of positional parameters (`m` in 7.5)
- `variadic`: set to `true` if the function has a variadic parameter (6.1). May be omitted if it does not.
- `type`: string representation of the function type (e.g., `Function<String, Number>`)

Example:

```json
{
  "$type": "FunctionRef",
  "module": "app/main.lask",
  "name": "greet",
  "arity": 1,
  "type": "Function<String, String>"
}
```

Rules for anonymous functions and closures:

- For function values without a stable reference name, `name` may be an implementation-generated identifier (e.g., `<lambda@L10C5>`).
- The actual values of closed-over free variables must not be included in `FunctionRef` by default.

### 13.3 Execution Events

```ebnf
ExecutionEvent = CallEvent | ReturnEvent | FailEvent .
CallEvent      = "call" EventCommon .
ReturnEvent    = "return" EventCommon .
FailEvent      = "fail" EventCommon ErrorInfo .
EventCommon    = TraceId FunctionRef ArgumentsSummary ResultSummary Timestamp .
```

Execution events follow the emission rules in 12.6 and must include the following fields in `json` or `pretty-json`.

- `kind`: `call` / `return` / `fail`
- `traceId`: correlation ID for the same execution
- `timestamp`: event time
- `function`: `FunctionRef`
- `args`: argument summary (required for `call`)
- `result`: return value summary (required for `return`)
- `error`: error information (required for `fail`)
- `env`: execution environment summary (environment kind `kind` and normalized parameter summary `params`. For a named environment (10.3), the reference name may be included as `name`)

Minimum fields of `ErrorInfo`:

- `code`: error code (Chapter 14)
- `message`: human-readable summary
- `detail`: optional details (within a range that can be safely disclosed)

Causal consistency rules:

- For the same function execution, exactly one `return` or `fail` must correspond after the `call`.
- `timestamp` should maintain non-decreasing order within the same trace.
- Child events of asynchronous execution share the same `traceId`, and parent-child relationship IDs may be attached as needed.

Execution events retain at least the following.

- Traceable identifier
- Target function
- Argument summary
- Return value summary
- Error information on failure

Example:

```json
{
  "kind": "fail",
  "traceId": "trc-01J8Y2...",
  "timestamp": "2026-07-11T12:34:56Z",
  "function": {
    "$type": "FunctionRef",
    "module": "ops/deploy.lask",
    "name": "deploy",
    "arity": 1,
    "type": "Function<String, String>"
  },
  "args": {"summary": ["prod"]},
  "env": {"kind": "remote", "params": {"host": "prod.example.com", "user": "deployer", "port": 22}},
  "error": {
    "code": "E-IO-SSH-CONNECT",
    "message": "ssh connection timeout",
    "detail": {"timeoutSec": 10}
  }
}
```

## 14. Error System

This chapter defines error classification and the minimum requirements for diagnostic information.

### 14.1 Error Classification

Errors in this specification are classified into the following 4 categories by the stage at which they occur.

- Syntax error: invalidity detected during lexical analysis and parsing
- Static error: invalidity detected during name resolution, type checking, and module resolution
- Runtime error: invalidity occurring during evaluation (division by zero, non-zero command exit, etc.)
- External I/O error: failure at external boundaries such as files, networks, SSH, and environment resolution

Classification rules:

- A single error must have exactly one primary classification.
- When there are compound causes, adopt the classification of the first observed root cause.

### 14.2 Error Code Conventions

Error codes take the form `E-<CATEGORY>-<DETAIL>`.

Category list:

- `SYNTAX`: syntax error
- `NAME`: name resolution error
- `TYPE`: type checking error
- `MODULE`: module resolution error
- `RUNTIME`: error during evaluation
- `IO`: external I/O error
- `CLI`: CLI usage error

Naming rules:

- Use only uppercase Latin letters and hyphens.

Representative codes:

- `E-SYNTAX-UNEXPECTED-TOKEN`
- `E-SYNTAX-RETURN-POSITION`
- `E-NAME-UNDEFINED`
- `E-NAME-AMBIGUOUS`
- `E-NAME-DUPLICATE`
- `E-TYPE-MISMATCH`
- `E-TYPE-ARITY`
- `E-TYPE-CALL`
- `E-TYPE-COMMAND-ENV`
- `E-TYPE-ENV-CONSTRUCT`
- `E-TYPE-ACCESS`
- `E-TYPE-FIELD-DUPLICATE`
- `E-TYPE-KEYWORD`
- `E-TYPE-ILLFORMED`
- `E-MODULE-CYCLE`
- `E-MODULE-UNRESOLVED`
- `E-MODULE-HASH-MISMATCH`
- `E-RUNTIME-DIV-BY-ZERO`
- `E-RUNTIME-COMMAND-NONZERO`
- `E-RUNTIME-AWAIT-FAILED`
- `E-RUNTIME-ACCESS`
- `E-RUNTIME-CAST`
- `E-IO-STDIN-READ`
- `E-IO-SSH-CONNECT`
- `E-IO-ENV-RESOLVE`
- `E-IO-DATA-DECODE`
- `E-CLI-USAGE`

### 14.3 Minimum Requirements for Diagnostic Information

An implementation must include at least the following when reporting an error.

- `code`: error code
- `message`: human-readable message
- `stage`: `syntax` / `static` / `runtime` / `io` / `cli`
- `traceId`: the identifier of 12.5, when available
- `location`: line and column, or span, where possible

Additional requirements related to types and names:

- For type mismatches, include `expected` and `actual`.
- For name resolution failures, candidates or a summary of the search scope may be included.

JSON format example:

```json
{
  "code": "E-TYPE-MISMATCH",
  "message": "argument type mismatch",
  "stage": "static",
  "traceId": "trc-01J8Y2...",
  "location": {"line": 12, "column": 8},
  "expected": "Number",
  "actual": "String"
}
```

### 14.4 Static Errors

Static errors are reported by pre-execution verification (Chapter 7).

Minimum targets:

- `E-NAME-UNDEFINED`
- `E-NAME-AMBIGUOUS`
- `E-NAME-DUPLICATE`
- `E-TYPE-MISMATCH`
- `E-TYPE-ARITY`
- `E-TYPE-CALL`
- `E-TYPE-COMMAND-ENV`
- `E-TYPE-ENV-CONSTRUCT`
- `E-TYPE-ACCESS`
- `E-TYPE-FIELD-DUPLICATE`
- `E-TYPE-KEYWORD`
- `E-TYPE-ILLFORMED`
- `E-MODULE-CYCLE`
- `E-MODULE-UNRESOLVED`

Rules:

- If there is one or more static errors, evaluation by `run` / `eval` must not be started.
- When multiple errors are detected, analysis may continue as far as possible so that they are reported together.

### 14.5 Runtime Errors

Runtime errors occur during evaluation under the dynamic semantics (Chapter 8).

Representative examples:

- `E-RUNTIME-DIV-BY-ZERO`: invalid arithmetic
- `E-RUNTIME-COMMAND-NONZERO`: failure caused by a non-zero exit of a command execution expression (`$`, `$1`, `$2`; 6.6)
- `E-RUNTIME-AWAIT-FAILED`: `await` re-raises a failed state
- `E-RUNTIME-ACCESS`: index out of range or missing key (8.9)
- `E-RUNTIME-CAST`: failure of the runtime type check of `cast` (15.8)

Rules:

- Runtime errors propagate from the point of occurrence to enclosing expressions (8.10).
- On failure, a `FailEvent` must be emitted according to the observation rules of Chapter 12.

### 14.6 External I/O Errors

External I/O errors occur at boundaries external to the language.

Representative examples:

- `E-IO-STDIN-READ`: stdin read failure
- `E-IO-ENV-RESOLVE`: execution environment resolution failure
- `E-IO-SSH-CONNECT`: SSH connection failure
- `E-IO-SSH-AUTH`: SSH authentication failure
- `E-IO-FS`: filesystem access failure
- `E-IO-DATA-DECODE`: failure decoding input data (stdin decoding in 9.4, `fromJson`/`decode` in 15.8)

Rules:

- External I/O errors may be reported with retryability information attached, where possible.
- Confidential information (keys, tokens, passwords) must not be included in `detail`.

### 14.7 Recoverability and Propagation Rules

Recoverability:

- `recoverable`: recoverable by retrying, correcting input, or changing configuration
- `non-recoverable`: a failure for which continuing the process is not appropriate

Default classification:

- Syntax and static errors are `recoverable` (assuming source correction)
- Runtime errors are implementation-defined, but `non-recoverable` by default
- External I/O errors are `recoverable` or `non-recoverable` depending on the cause

Propagation rules:

- Expressions containing `do` / collection functions (`map`, `filter`, `reduce`, `forEach`) / `await` follow the failure propagation rules of 8.10.
- Unless explicitly caught by `try` / `catch` (6.9), a failure propagates to the top level, and the process exits with the `code` of the `Error` value (8.10, 11.3).
- The `recoverable` / `non-recoverable` classification is information for diagnostics and operations, and does not affect catchability. Both runtime errors and external I/O errors occurring during evaluation can be caught.

### 14.8 Correspondence to CLI Exit Codes

The CLI follows the exit code contract of 11.3 and maps error classifications as follows.

- Syntax and static errors -> `1`
- Runtime errors -> the `code` of the uncaught `Error` value (default `2`. Command failures pass through the child process's exit code. 8.10)
- External I/O errors -> the `code` of the uncaught `Error` value (default `3`)
- CLI usage errors -> `4`

Rules:

- When multiple classifications exist simultaneously, adopt the exit code of the fatal classification that occurred first.
- With `--format json`, an error object corresponding to the exit code must be output to stderr.
- Exit code normalization (values outside 1-255 become `1`) and the handling of exit code overlap follow 11.3.

## 15. Built-in Library

This chapter defines the functions, operators, and environment operations provided by the language implementation by default.

### 15.1 Provision Policy

The built-in library is the set of built-in symbols usable without explicit import.

Policy:

- Referentially transparent pure functions are preferred.
- Functions with external side effects are clearly distinguished by name and contract.

Publication rules:

- Symbols of the built-in library are positioned at the lowest level of name resolution (the 5th rank in 7.2). If a user definition declares a symbol with the same name, it may shadow it according to the ordinary name resolution rules.
- However, core function names specified as "must not be overridden" in Chapter 6 cannot be declared or bound (7.2).

Typing rules:

- Type variables appearing in the signatures of this chapter (`T`, `U`, etc.) follow the built-in polymorphism rules of 4.4. Only built-in symbols can have polymorphic types; type variables cannot be used in the signatures of user-defined functions.

### 15.2 Numeric Operations

The built-in library provides at least the following functions.

- `add`: `Function<Number, Number, Number>`
- `sub`: `Function<Number, Number, Number>`
- `mul`: `Function<Number, Number, Number>`
- `div`: `Function<Number, Number, Number>`
- `mod`: `Function<Number, Number, Number>`
- `abs`: `Function<Number, Number>`
- `floor`: `Function<Number, Number>`
- `ceil`: `Function<Number, Number>`
- `round`: `Function<Number, Number>`

Semantics:

- `add`/`sub`/`mul`/`div`/`mod` have meaning equivalent to the arithmetic operators of 6.2.
- If the divisor of `div` is 0, it is `E-RUNTIME-DIV-BY-ZERO`.
- If the right-hand side of `mod` is 0, it is also `E-RUNTIME-DIV-BY-ZERO`.

### 15.3 String Operations

The built-in library provides at least the following functions.

- `length`: `Function<String, Number>`
- `concat`: `Function<String, String, String>`
- `trim`: `Function<String, String>`
- `toLower`: `Function<String, String>`
- `toUpper`: `Function<String, String>`
- `split`: `Function<String, String, Array<String>>`
- `join`: `Function<Array<String>, String, String>`
- `replace`: `Function<String, String, String, String>`

Semantics:

- Strings are treated as UTF-8.
- `length` may count in units of characters, but the implementation must keep the counting rule consistent.
- The behavior of `split`/`join` when the delimiter string is empty may be implementation-defined.
- `concat` concatenates 2 strings. To concatenate 3 or more, use nested applications of `concat` or `join`.
- String concatenation is done with `concat` (or `join`). `+` is exclusive to `Number`, and applying it to `String` is a type error (6.2).

### 15.4 Array, Map, and Record Operations

The built-in library provides at least the following functions.

- `map`: `Function<Array<T>, Function<T, U>, Array<U>>`
- `filter`: `Function<Array<T>, Function<T, Bool>, Array<T>>`
- `reduce`: `Function<Array<T>, U, Function<U, T, U>, U>`
- `forEach`: `Function<Array<T>, Function<T, U>, Void>`
- `append`: `Function<Array<T>, T, Array<T>>`
- `concatArray`: `Function<Array<T>, Array<T>, Array<T>>`
- `get`: `Function<Map<T>, String, T>`
- `hasKey`: `Function<Map<T>, String, Bool>`
- `keys`: `Function<Map<T>, Array<String>>`
- `values`: `Function<Map<T>, Array<T>>`

Semantics:

- `map`/`filter`/`reduce`/`forEach` traverse the input array from left to right (evaluation rules in 8.5).
- `reduce` requires an initial value.
- `forEach` discards each application result and returns `Void`. It is used for iteration whose purpose is side effects.
- `map`, `filter`, `reduce`, and `forEach` are core functions that include the normalization targets of the control structures of 6.4, and must not be overridden by user code (subject to the exception provision of 15.1).
- `get` results in a runtime error (`E-RUNTIME-ACCESS`) when the key is absent. This is the same failure contract as index access `m[k]` (6.8, 8.9). To tolerate a missing key, check in advance with `hasKey`.

### 15.5 Command Execution Functions

The built-in library provides at least the following functions.

- `runCommand`: `Function<String, CommandResult>` (with keyword parameter `--env: Environment = #local`)

Contract:

- Follows the rules of 6.6, 8.7, and Chapter 10. `CommandResult` is the built-in type alias defined in 6.6.
- `runCommand` succeeds regardless of the exit code as long as the command completes. The diagnostic code for failures caused by a non-zero exit of the command execution expressions `$`, `$1`, and `$2` (6.6) is `E-RUNTIME-COMMAND-NONZERO`.
- Environment resolution failure is `E-IO-ENV-RESOLVE`.

### 15.6 Parallel and Asynchronous Helper Functions

The built-in library provides at least the following functions.

- `spawn`: `Function<Function<T>, AsyncHandle<T>>`
- `await`: `Function<AsyncHandle<T>, T>`
- `all`: `Function<Array<AsyncHandle<T>>, Array<T>>`
- `race`: `Function<Array<AsyncHandle<T>>, T>`

Semantics:

- `spawn`/`await` follow the rules of 6.3 and 8.6.
- Because `await` is a reserved word, it can only be applied in the `AwaitExpr` form (`await h`); when passing it as a function value, write `\(h) -> await h` (6.3).
- `all` waits for all handles to complete and returns the result array in input order.
- `race` returns the first result to succeed or fail.

Failure rules:

- `all` may fail as soon as any single one fails.
- Receiving a failed result via `await` is `E-RUNTIME-AWAIT-FAILED`.

### 15.7 Error Handling Functions

The built-in library provides at least the following functions.

- `recover`: `Function<Function<T>, Function<Error, T>, T>`
- `fail`: `Function<Error, T>`
- `error`: `Function<Number, String, Error>`

The `Error` type:

- `Error` is the built-in type alias `Record<code: Number, message: String>` (6.9). It must not be overridden by user definitions.

Semantics:

- `recover` / `fail` follow the rules of 6.9 and 8.10. Both are core functions and must not be overridden by user code (7.2).
- `error(code, message)` is a helper function that constructs an `Error` value equivalent to `{code: code, message: message}`.
- The return type `T` of `fail` is concretized from the context's expected type via built-in polymorphism (4.4).

Failure rules:

- `fail` raises a failure carrying the given `Error` value. If uncaught, the process exits with `code` as the exit code (8.10, 11.3).

### 15.8 Serialization and Type-Migration Helper Functions

The built-in library provides at least the following functions.

- `toJson`: `Function<Any, String>`
- `fromJson`: `Function<String, Any>`
- `encode`: `Function<Any, String, String>`
- `decode`: `Function<String, String, Any>`
- `cast`: `Function<Any, T>`

Format arguments:

- The 2nd argument of `encode`/`decode` is a format specification string, and at least `json` and `pretty-json` are accepted.

JSON conversion rules of `fromJson` / `decode`:

- JSON number -> `Number`
- JSON string -> `String`
- JSON boolean -> `Bool`
- JSON null -> `Null`
- JSON array -> array value (elements have these rules applied recursively)
- JSON object -> `Record<...>` or `Map<Any>` (implementation choice. However, it must be consistent within the same implementation)
- Tagged metadata representations (objects with a `$type` field) are also not treated specially, and are converted as ordinary objects under the above rules. Restoration of `Void`, `Environment`, and `FunctionRef` is not performed (round-trip rules of 13.1).

Type migration via `cast`:

- `cast(v)` checks at runtime whether the value `v` conforms to the target type `T`, and if it conforms, returns `v` as a value of type `T`. It is the sole means of migration from `Any` to a concrete type (4.4).
- The target type `T` must be uniquely concretized from the expected type at the reference position via built-in polymorphism (4.4) (e.g., `user: Record<name: String> = cast(fromJson(stdin))`).
- The target type `T` is limited to data types (`Number`, `String`, `Bool`, `Null`, `Environment`, `Any`, and `Array`/`Map`/`Record` composed of them). A `cast` to a type containing `Void`, `Function`, or `AsyncHandle` is a static error (`E-TYPE-ILLFORMED`).

Runtime type check rules for `cast`:

- Basic types: the kind of the runtime value matches the target type.
- `Array<T>`: the value is an array value and every element conforms at runtime to `T`.
- `Map<T>`: every value conforms at runtime to `T`.
- `Record<...>`: the key set matches the field set of the target type, and each value conforms at runtime to the corresponding field type.
- Record values and map values are mutually acceptable. When the structural conditions are satisfied, the implementation converts to the target type's representation (record or map) as needed (to absorb the implementation choice for JSON objects in `fromJson`).
- Positions of `Any` within the target type pass without checking.
- If the check fails, it is a runtime error (`E-RUNTIME-CAST`). The diagnostics should include the failing position (field path, index).

Contract:

- Follows the Serialization Conventions of Chapter 13.
- Invalid JSON is reported as `E-IO-DATA-DECODE`.
- An unsupported format specification is reported consistently as either `E-CLI-USAGE` or `E-IO-DATA-DECODE`.

### 15.9 Error Contract

Functions of the built-in library must be consistent with the Error System of Chapter 14.

Minimum requirements:

- Generate a diagnostic with `code` and `message` on each failure path.
- Attach `location` and `traceId` where possible.
- Maintain determinism, returning the same error code for the same condition.

## 16. Examples

This chapter presents representative examples spanning the whole specification. Each example lists the command and expected behavior, assuming the code is placed in `main.lask` in the execution directory. Since `run` does not output the evaluation result to stdout (11.3), use `eval` to confirm values. Output examples use the default `json` display.

### 16.1 Minimal Program

A minimally structured module example is shown.

```lask
hello() = "hello, lask"
```

This module publishes one zero-argument function `hello`, returning a `String` value.

Command and expected behavior:

```text
$ lask eval hello
"hello, lask"

$ lask run hello
```

- `eval` outputs the evaluation result to stdout and exits with exit code `0`.
- `run` executes the function but does not output the evaluation result to stdout (execution logs go to stderr). It exits with exit code `0`.

### 16.2 Functions with Type Annotations

An example of functions with type annotations and default arguments is shown.

```lask
greet(name: String, --prefix: String = "hello"): String =
  concat(prefix, concat(", ", name))

add(x: Number, y: Number): Number =
  x + y
```

`greet("alice")` returns `"hello, alice"`, and `greet("alice", prefix = "hi")` returns `"hi, alice"`.

Command and expected behavior:

```text
$ lask eval greet alice
"hello, alice"

$ lask eval greet alice --prefix hi
"hi, alice"

$ lask eval add 1 2
3

$ lask run greet alice --prefix hi
```

- `name` is a positional parameter and is bound only by position. Specification by name such as `--name alice` is not possible (a CLI usage error corresponding to `E-TYPE-KEYWORD`).
- `prefix` is a keyword parameter and is bound only by name. When unspecified, it is completed with the default value `"hello"` (7.5).
- The arguments of `add` are bound positionally as `Number` values, with `1` and `2` decoded by the default `--arg-decode auto`.
- The last `run` example executes with the same syntax and binding as `eval`, but the evaluation result is not output to stdout, and it exits with exit code `0`.
- `lask run greet` without the positional parameter `name` is a pre-execution error and the function is not evaluated (CLI usage error; exit code `4`; 11.3).

### 16.3 Higher-Order Functions and Composition

Examples of higher-order functions, the pipe operator, and the composition operator are shown.

```lask
applyTwice(f: Function<Number, Number>, x: Number): Number =
  f(f(x))

inc(x: Number): Number = x + 1
double(x: Number): Number = x * 2

pipeline(n: Number): Number =
  n
  |> inc
  |> double

incThenDouble: Function<Number, Number> =
  inc >> double
```

`pipeline(3)` and `incThenDouble(3)` both return `8`.

Command and expected behavior:

```text
$ lask eval pipeline 3
8

$ lask eval incThenDouble 3
8
```

- `incThenDouble` is a top-level declaration of a function value, and can be invoked from the CLI just like a function declaration. Since this is application of a value of function type, binding is by positional arguments only (7.5).
- `applyTwice` has a parameter of function type, so it cannot be invoked directly from the CLI (no decoding scheme can construct a function value, so it becomes a pre-execution error due to type mismatch). Calling `applyTwice(inc, 3)` from within the language returns `5`.

### 16.4 Arrays, Maps, and Records

Examples of array, map, and record values, and the use of built-in library functions and accessor expressions (6.8) are shown.

```lask
labels(xs: Array<String>): Array<String> =
  map(xs, \(x: String) -> concat("item:", x))

firstLabel(xs: Array<String>): String =
  labels(xs)[0]

user: Record<name: String, age: Number> =
  {name: "alice", age: 20}

userName(): String = user.name

cfg: Record<env: Environment, retries: Number> =
  {env: #alpine:3.20, retries: 3}

retryCount(): Number = cfg.retries

envMap: Map<String> =
  {"APP_ENV": "prod", "REGION": "ap-northeast-1"}

appEnv(): String = envMap["APP_ENV"]

region(): String =
  get(envMap, "REGION")
```

Command and expected behavior:

```text
$ lask eval labels '["a", "b"]'
["item:a", "item:b"]

$ lask eval firstLabel '["a", "b"]'
"item:a"

$ lask eval userName
"alice"

$ lask eval retryCount
3

$ lask eval appEnv
"prod"

$ lask eval region
"ap-northeast-1"
```

- The argument of `labels` is interpreted as a JSON array by the default `--arg-decode auto` and bound to `Array<String>`.
- `user.name` and `cfg.retries` are record field accesses (`.` is exclusive to `Record`; 6.8). Referencing a nonexistent field is a static error (`E-TYPE-ACCESS`).
- `labels(xs)[0]` is array index access (0-based). An out-of-range index is a runtime error (`E-RUNTIME-ACCESS`; 8.9).
- `envMap["APP_ENV"]` is map index access, and has the same failure contract as `get(envMap, "APP_ENV")` (a missing key is `E-RUNTIME-ACCESS`). `region` performs the same retrieval using the built-in library's `get`.
- `user`, `cfg`, and `envMap` are value declarations that are not functions, and are not targets of direct invocation from the CLI.

### 16.5 Procedural Notation and Command Execution with Environments

Examples of `do`, `if`, `for`, and `$[env] ...` are shown.

```lask
buildAndTest(env: Environment): String = do {
  buildOut = $ sh -lc "echo local-build"

  testOut = if (length(buildOut) > 0) {
    $[env] sh -lc "echo container-test"
  } else {
    "skip"
  }

  lines = for (x : ["lint", "unit", "e2e"]) {
    concat("phase:", x)
  }

  concat(join(lines, ","), concat(":", trim(testOut)))
}

ci(): String = buildAndTest(#alpine:3.20)
```

Command and expected behavior:

```text
$ lask run ci

$ lask eval ci
"phase:lint,phase:unit,phase:e2e:container-test"
```

- In both cases, `echo local-build` is executed locally, followed by `echo container-test` executed inside the `alpine:3.20` container. `run` does not output the evaluation result, and `eval` outputs the return value to stdout.
- `buildAndTest` has a parameter of type `Environment`, so it is excluded from direct CLI invocation (11.2). It is invoked via a function with the environment fixed, like `ci`.
- If any command exits non-zero, the failure propagates and `lask` exits with that command's exit code (8.10, 11.3).

During execution, a command execution log like the following flows to stderr in real time (12.3):

```text
2026-07-23T10:15:04.101Z [#local:1] $ sh -lc "echo local-build"
2026-07-23T10:15:04.113Z [#local:1] 1| local-build
2026-07-23T10:15:04.114Z [#local:1] exit 0
2026-07-23T10:15:04.360Z [#alpine:3.20:2] $ sh -lc "echo container-test"
2026-07-23T10:15:04.372Z [#alpine:3.20:2] 1| container-test
2026-07-23T10:15:04.373Z [#alpine:3.20:2] exit 0
```

### 16.6 Asynchronous Execution

Corresponding examples of `async` (sugar) and `spawn`/`await` (core functions) are shown.

```lask
fetchA(): String = $ sh -lc "echo A"
fetchB(): String = $ sh -lc "echo B"

mergeAB(): String = do {
  a = async fetchA()
  b = async fetchB()
  concat(trim(await a), trim(await b))
}
```

Semantically, this example normalizes to `spawn(\() -> fetchA())`, `spawn(\() -> fetchB())`, and core function applications `await(...)`.

Command and expected behavior:

```text
$ lask eval mergeAB
"AB"
```

- `fetchA` and `fetchB` are started concurrently by `async`, and both completions are awaited via `await` before concatenation. Regardless of the completion order of the 2 commands, the result is always `"AB"`.
- If either asynchronous task fails, the failure is re-raised at the corresponding `await` (8.6).

### 16.7 CLI Execution Examples

```text
# Type checking
lask check --module app/main.lask

# Local execution (main.lask is the default target. The result is not output to stdout)
lask run hello
lask run --module app/main.lask hello

# Function evaluation (outputs the evaluation result to stdout)
lask eval greet alice

# Execution with keyword arguments (greet(name: String, prefix: String = "hello"))
lask run greet alice --prefix hi

# Fixed-image execution (build specifies $[#alpine:3.20] inside the function)
lask run --module ci/main.lask build

# Execution in a remote environment (provision references #env("ansible"). Environment definitions are in environments.lask.json)
lask run --module ops/main.lask \
  --ssh-known-hosts ~/.ssh/known_hosts \
  --ssh-strict-host-key-checking yes \
  --ssh-connect-timeout 10 \
  provision

# Enumerating available environments and checking access
lask envs provision --check
```

Expected behavior:

- `lask check`: outputs diagnostic results to stdout. Exit code `0` if there are no static errors, `1` if there are.
- `lask run hello`: executes the function and exits with exit code `0` without outputting the evaluation result to stdout.
- `lask eval greet alice`: outputs `"hello, alice"` to stdout (the default `json` display).
- `lask run greet alice --prefix hi`: executes with the positional argument `alice` and the keyword argument `--prefix hi` bound. The evaluation result is not output.
- `build`: executes the build inside a container (the evaluation result is not output). If a connection to the Docker daemon cannot be made, a pre-execution error occurs and it exits with exit code `3`.
- `provision`: resolves to the `ansible` entry in `environments.lask.json` and executes commands over an SSH connection. If the connection fails, an `E-IO-SSH-CONNECT` diagnostic is output to stderr and it exits with exit code `3`.
- `lask envs provision --check`: enumerates the environments used by `provision` and checks reachability. Exit code `0` if all are reachable, `3` if any environment is unreachable (11.4).

### 16.8 Execution Event Example

An event output example on failure involving command execution with environments (`runCommand`) is shown. The following is the `FailEvent` emitted when the SSH connection times out during execution of `lask run deploy prod`.

```json
{
  "kind": "fail",
  "traceId": "trc-20260711-0001",
  "timestamp": "2026-07-11T12:34:56Z",
  "function": {
    "$type": "FunctionRef",
    "module": "ops/main.lask",
    "name": "deploy",
    "arity": 1,
    "type": "Function<String, String>"
  },
  "args": {"summary": ["prod"]},
  "env": {"kind": "remote", "params": {"host": "prod.example.com", "user": "deployer", "port": 22}},
  "error": {
    "code": "E-IO-SSH-CONNECT",
    "message": "ssh connection timeout",
    "detail": {"timeoutSec": 10}
  }
}
```

Expected behavior:

- If this failure is not caught by `try`/`catch`, the failure is mapped to an `Error` value (`code: 3`), and the `lask` process exits with exit code `3`. An `E-IO-SSH-CONNECT` diagnostic is output to stderr (8.10, 11.3).
- The `FailEvent` is emitted regardless of whether the failure is caught (12.6).

### 16.9 Error Handling and Exit Codes

An example of `try` / `catch` / `finally` and the pass-through of exit codes and standard error is shown.

```lask
release(): String = do {
  out = try {
    $ ./release.sh
  } catch (e) {
    if (e.code == 75) {
      "retry-later"
    } else {
      fail(e)
    }
  } finally {
    $ rm -rf ./tmp
  }
  out
}
```

Command and expected behavior:

```text
$ lask run release
```

- On success, it exits with exit code `0` (the return value is not output to stdout. To confirm the value, use `lask eval release`). The `finally` block ensures `./tmp` has been deleted.
- If `./release.sh` fails with exit code `75`, the failure is caught, `"retry-later"` is returned, and it terminates normally (process exit code `0`).
- Other failures are re-raised by `fail(e)`. If it reaches the top level uncaught, the `lask` process exits with that command's exit code, and `e.message` (the command's standard error output) is output as diagnostics (8.10, 11.3).
- The `finally` block is evaluated exactly once on every path: success, catch, or re-raise.
