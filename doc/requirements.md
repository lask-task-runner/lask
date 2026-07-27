# Requirements Definition Document

This document summarizes the requirements for "Lask", a functional-style task runner and CI/CD tool.

## 1. Overview

Lask is a new task runner and automation tool for developers that aims to be "locally verifiable", "type-safe", and "highly modular".
It provides the simplicity of Makefiles, the task management capabilities of Task/Just, and the reproducibility of container integration found in tools like Earthly/Dagger, through a simple functional DSL and a powerful ecosystem (LSP, etc.).

## 2. Problems to Solve (Background)

Existing CI/CD tools and task runners suffer from the following problems.

1. **Difficulty of local testing (Push and Pray)**
   - Configurations for many CI/CD platforms (YAML, etc.) are difficult to reproduce and run in an identical local environment, leading to significant time lost repeating the cycle of failing remotely and fixing.
2. **Script sprawl and low reusability**
   - Large shell scripts and Makefiles are difficult to modularize, and it is hard to reuse logic across projects. Managing arguments and default values also tends to become cumbersome.
3. **Lack of static verification**
   - Mechanisms for detecting errors before execution (missing arguments, typos, type checking) are weak, and problems are often noticed only when they become runtime errors.
4. **Implicit dependence on the execution environment (host)**
   - Results vary depending on packages installed on the host machine ("It works on my machine").
5. **Retention of execution history**
   - Mechanisms for storing and referencing execution logs and artifacts as CI/CD execution history are weak.

## 3. Core Requirements (Value Proposition)

To solve these problems, Lask satisfies the following core requirements.

### 3.1 Declarative, composable, functional-based syntax
- Tasks and jobs must be definable as "functions" that can be called from other functions and whose results can be composed.
- Argument systems equivalent to modern programming languages must be supported, including positional arguments, keyword arguments, variadic arguments, and default arguments.

### 3.2 Transparent container integration of the execution environment
- In addition to execution on the host environment, it must be possible to seamlessly specify task execution inside containers at the language level. This guarantees environment reproducibility while preventing configuration file bloat.

### 3.3 Robust pre-execution static analysis
- Static syntax and type analysis must be provided to detect problems such as missing arguments and type inconsistencies before runtime execution.

### 3.4 Advanced editor support (DX)
- Through LSP (Language Server Protocol) and similar mechanisms, the system must be structured so that powerful development support such as completion, hover, and error highlighting is available in editors.

### 3.5 High modularity and reusability
- File splitting and importing functionality from other modules must be facilitated, so that CI/CD scripts, which tend to grow large, can be split into units reusable across teams and projects.

## 4. Functional Requirements

### 4.1 Language and Syntax
1. **Unified expression-based definitions**: All elements, including tasks and jobs, can be declared as equivalent expressions.
2. **Basic data schema**: Basic data types such as booleans, numbers, strings, arrays, and objects must be provided.
3. **Environment reference type**: A type representing execution environments (container images, etc.) must exist and be handled as an in-language object.
4. **Control structures**: Flow control syntax required for task execution must be provided, such as conditional branching (if/else) and iteration/filter operations over collections.
5. **Function control**: First-class functions and closures must be supported to maintain reusability.
6. **Module functionality**: An import mechanism must be provided to load functions and variables from other files and packages and use them without polluting the global namespace.
7. **Shell-integrated operation syntax**: Syntax support must be provided for concisely writing command invocations and shell execution.
8. **Expression interpolation mechanism**: Variables and expressions must be easily interpolated into strings inside string and command notations.

### 4.2 Interface Functionality (CLI/API)
1. **Task execution**: A specified function (task) in a script can be executed.
2. **Interactive environment (REPL)**: An interactive prompt (REPL environment) is provided for evaluating and checking commands and functions on the spot.
3. **Expression evaluation and output**: A specified function can be evaluated and its execution result output in a format that is easy for other tools to interpret (JSON, etc.).
4. **Static verification**: Syntax and reference errors in the code of a script can be verified statically.
5. **Type inference**: Type inference results for variables and functions are provided.
6. **Editor integration**: A server capability for editor integration is provided.

### 4.3 Execution and Evaluation Functionality
1. **Name resolution and scope**: Scope resolution must be performed consistently based on the precedence of local bindings such as arguments, the current module, imported modules, and built-in functions (built-ins have the lowest precedence, and shadowing by user definitions is permitted).
2. **Parallel task execution**: A parallelism mechanism must be provided to evaluate task calls and groups of shell commands with no dependencies concurrently for efficient execution.
3. **I/O management**: I/O must be handled for command execution (local/container, etc.), and streams (standard output/error) must be routed appropriately.
4. **Ingestion of external input**: A mechanism must be provided to transparently reference and use runtime command-line arguments and standard input within scripts.

## 5. Non-functional Requirements

1. **Performance**
   - Analysis and inference must be sufficiently fast, with responsiveness that does not interrupt the user's train of thought even during editor integration (responses within tens of milliseconds).
2. **Portability**
   - The tool must be distributable and executable cross-platform (macOS, Linux environments, etc.) without depending on the execution infrastructure.
3. **Extensibility**
   - The executor model must be designed generically so that built-in libraries and function sets can be added or replaced, and language functionality can be easily extended.

## 6. Required Specifications

This section describes the specifications that must be satisfied.

* **Language paradigm**
  * While based on functional programming, a procedural writing style must also be supported so that users accustomed to Makefiles and shell scripts can write naturally.
  * Lambda expressions and higher-order functions must be supported.

* **Function calls and arguments**
  * Functions can be invoked from the command line.
  * Function arguments must be specifiable naturally, following the conventions of common CLI tools.
  * Arguments specified via the CLI must be bindable to keyword parameters on the function definition side.
  * The interpretation and decoding method of arguments must be specifiable.

* **Data types and serialization**
  * Input and output of data types must be handled in a serializable and human-readable format.
  * Function values (including lambdas and closures), Void, and execution environment values (Environment) must not be direct serialization targets; they must be handled as metadata representations referenceable in execution records.
   * Supported types must be defined separately as basic types and composite types.
   * Basic types:
      * Any: A general-purpose type that accepts any value.
      * Number: A numeric type handling integers and floating-point numbers.
      * String: A string type handling UTF-8 strings.
      * Bool: A boolean type handling true/false.
      * Null: A type indicating the absence of a value.
      * Void: A type indicating that no return value exists.
   * Composite types:
      * Array<T>: An array type with an element type. When the element type is Any, mixed elements are permitted.
      * Map<T>: A map type with string keys (keys are always String).
      * Record<...>: A record type with fixed fields.
      * Function<Args..., R>: A function type with argument types and a return type (including lambda expressions and higher-order functions).
   * Special types:
      * Environment: A built-in type representing an execution environment (local/container/remote). It has a special standing distinct from basic types: values can only be constructed by environment expressions, and serialization is limited to a metadata representation.
   * Note: The error value type Error is not a new basic type; it is provided as a built-in record type alias composed of basic types (Record<code: Number, message: String>).

* **Standard I/O and pipes**
  * Data must be passable between functions through standard I/O.
  * A scheme in which standard input is received until EOF and then passed to function evaluation must be supported; receiving during execution is out of scope.
  * Standard input must be received as a UTF-8 string, and structuring such as line splitting or JSON interpretation must be performed explicitly with in-language standard functions (split, fromJson, etc.). No mechanism is provided to switch the decoding method via runtime options (to prevent the meaning of the program from changing per invocation).

* **Execution model and error handling**
  * Execution must be sequential by default while allowing parallel execution.
   * Means for specifying asynchronous invocation and waiting must be provided.
  * Normally, execution must terminate immediately when a runtime error occurs.
  * A mechanism for recovering from errors must be provided.
  * On failure, an error code and an error message must be returned.

* **Type system policy**
  * Type annotations must be optional, and types must be determinable by type inference.
  * Overloading is not supported.
  * Abstract types are not supported.

* **Execution environment**
  * The execution environment must be specifiable at command invocation.
  * The local environment, Docker containers, remote servers, and the like must be targetable as execution environments.

* **Observability and execution records**
  * Stack traces must be recordable at all times.
  * Execution logs must be outputtable to standard error.
  * Function execution results must be outputtable to standard output.
  * The output encoding method must be specifiable (a default must be defined).
  * Function calls, return values, and failures must be recordable as chronological events.
  * Execution events must be able to carry a traceable identifier, the target function, summaries of arguments and return values, and error information on failure.
  * When arguments or return values contain function values, they must be recorded as reference information rather than the value itself.
