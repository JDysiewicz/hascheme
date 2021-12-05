# HaScheme

This is an Scheme implementation written in Haskell using the "Write Yourself a Scheme in 48 Hours" resource: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours. I relied quite heavily on this tutorial, however it taught me about many concepts in Haskell that I was only vaugley aware of. I also implemented some refactors, such as splitting the codebase up from a single `Main.hs` file into appropriately named modules.

## Usage

Run with `cabal run hascheme` for REPL, or `cabal run hascheme "<INPUT>"` to eval a single line of code to run from Cabal.

To run executable REPL, cd into `app` and run the executable `hascheme` there (cd into dir first, else pathing for loading stdlib is messy). To load stdlib in,
use command `(load "stdlib.scm")` in the REPL.

## Struggles

This is not an introduction to Haskell resource, and so it doesn't pull any punches about introducing potentially unfamiliar concepts early on. However, this exposed me to a lot of things I was not aware of, and so gave me a great jumping off point. In general that was how I approached this task: read the next step, be confused about the concepts/code, go away and research until I could fully understand what was happeneing in the code, then implement the code and continue. Documented here are a **few** of the hurdles I faced. This was my first time attempting something relatively low-level, and doing so in Haskell was relatively challenging; I hope to use these learnings to make some more interesting projects in the future (e.g. fleshing this out to be a complete Scheme implementation, creating my own toy language, looking into writing a compiler, etc).

### Parsing

My knowledge of parsers, specifically combinator parsers, was near-zero. And this project starts right out the gate introducing combinatory parsers and some advanced usages. Combine that was some relatively forieign syntax (e.g. `<|>` or the "_choice_" oeprator) and I was lost quite early on. This led me to try and find everything I could about understanding parsers and how they function. Eventually, the heavily recursive nature of them clicked somewhat for me and I began to understand how `Parsec` works. I know I've barely scratched the surface of them, however.

### Monad Transformers

The combination of the IO monad with the Error monad via a monad transformer was lost on me for a while, having just wrapped my head around what a monad is. However, the explanation that monad transformers are a way of combining functionality of monads helped quite a bit.

### Environments

Using IORef to create a mutable map to simulate state was my first forary into stateful programming in Haskell. This made me broaden my view on what state is a little more, as, while this is impure, it is still trapped within the context of an IORef and so preserves safety.

### Recursion

I am no stranger to recursion, and I am thinking in terms of it more and more these days (thanks to using Haskell for AoC this year). However, so much of parsing/interpreting/implementing a language is recursive in nature and so can be difficult to gain a full view of everything going on.

### Syntax

It seems somewhat silly to say this was a hurdle, however many of the implementations used in WYAS were quite heavy in terms of advanced syntax use. Many cases where `>>=`'s are chained together into complicated monadic pipelines. I still struggle to understand these somewhat, however I'm getting there.

### Scheme Understanding

Scheme has a relatively simple grammar and syntax, however I steadily realised I'd need to invest more time into learning it before continuing with this project. While I'm by no means proficient in it, I started reading "Structure and Interpretations of Computer Programs" again to refresh my knowledge; specifically I was lacking information about how scheme treats lists/arrays as literals using quoting.
