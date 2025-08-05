# Lask
Lask (coined by combining "lambda" and "task") is a task runner based on functional programming.

### Installation

To build Lask, you need to install the Haskell toolchain.
The recommended way is to use [GHCup](https://www.haskell.org/ghcup/).

To build Lask, you can use the following command:

```bash
$ stack --local-bin-path /usr/local/bin/ install
```

### Usage

To run Lask, you can use the `lask` command in your terminal.

```bash
$ lask --help
```

### Example

To show a simple example, here is a basic Lask script that prints "Hello, World!":

```bash
$ cd ./example/01-basic
$ lask run hello

Hello, World!
```
