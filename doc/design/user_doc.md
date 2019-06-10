# Introduction

- Lowering cognitive load + visual & textual representation are equal
- Simplicity and correctness

## Do we need a new programming language?

- the coming software apocalypse
- difference between Enso and other programming languages - why not visualisation library for Python etc
- Modern problems needs modern solutions 



## "Immediate connection to the data" 

- Brett Victor blogpost introduction

## High-level design guideline (dedicated for libraries authors)

- visual and textual layers are equal
- think always: visual first!
- there should be one way to achieve a goal

# Syntax

- Where to use what syntax - info that textual syntax is above nodes – this are the **same expressions**.

## Encoding

- why we support UTF8

## Layout

- Dedicated IDE for Enso is always providing autolayouting tools. 

- There is no strict rules about graph shape or layout since this is artistical expression for the process but for textual representation we should name a few:

  ### maximum line length

  - 80 chars

  ### indentation

  - writing code with indentation blocks and how it affects the program

  ### debugging (maybe should go to functions description?)

  - `#=` for graph and `//` for text

## Naming Rules (maybe this should be sneaked in Functions chapter?)

- You are free to use capitalized and uncapitalized identifiers as function and variable names. Type definition identifier should always be capitalized.
- Capitalized and uncapitalized identifiers are not distinguishable and always refer to the same value.
- Using capitalized identifier to refer to uncapitalized one is allowed in pattern matching only. Using uncapitalized identifier to refer to capitalized one is disallowed.
- In pattern matching capitalized identifiers refer to values in the scope, while uncapitalized identifiers are used for free variables only.

# Type System

- A *type system* is a set of rules that assigns a property called *type* to the various constructs of a computer program, such as variables, expressions, functions or modules [link do wikipedi].

- Enso is a statically typed language. 

- Basic types and datatypes - just first informations + what basic types are available in Enso

## Type Signatures

- In Enso it is not needed to provide type signatures but it is useful to provide additional informations for the compiler

- to provide explicit type signature for a variable/function:

  ```
  sum: Int -> Int -> Int
  sum a b = a + b
  ```

- option - explicit type information with colon operator

# Data Types - description

- what a data type is and why you should use it powers
- avoiding mistakes
- custom visualizations
- better hints

## Type Constructors

- how to create new data type with constructor

## Algebraic Data Types

- **algebraic data type**  introduction and explanation
- example of algebraic data type

- syntax sugar for algebraic data types in Enso

# Pattern Matching

- match against simple values
- destructing complex data types
- examples

# Variables

- Variables in statically typed language are different than in imperative. - why and how to use it
- how to define variable in node and text editor
- how to express the type of a variable in node and text editor
- pattern matching if you want to bring variable to scope 

# Functions

## how to create a function

- how to define function in node editor 
- how to define function in text
- function with arguments  

## First class and higher-order functions

- what a first class function is
- how to define higer-order function
- partial application 
- currying

## Function Type

- just like for variables we can provide additional informations about function by explicit typing - how to define it in node and text editors
- arity - as an extra information not for data scientists 

## Arguments

- Named Arguments

- Default arguments

- Positional arguments

- Optional Arguments

- Splat Arguments
- Important: code snippets for each from the domain we want to support the most

## Case and If

- `case` allows us to compare a value against many patterns until we find a matching one
- check conditions using `if ... then ... else ...`

- `do` blocks

## Uniform Calling Syntax (UCS)

- two functional call notations: `lst.map +1` and `map +1 lst`
- first automatically supported in node editor and preferred 

## Recursion

- in impreative languages usually use loops - in functional languages we have recursion  - what a recursion is and how to use it - example loop-recursion side by side
- reduce (accumulating parameters)

## Lazy/strict

- what is laziness 
- why sometimes you may want to use it
- how to define lazy fields

# Operators

- list and describe operators predefined in Enso with examples:
  - `|>`
  - `<|`
  - `>>`
  - `<<`

## How to define custom operator

- example and step by step explanation

## More about operators - precedence

- spaces matters and the precedence change depending on spaces

# Visualizations

- how to define specific visualization for a data type
- how to create custom visualization
- how to connect with a new visualizations library (probably link to some blog post etc)

# Errors handling

- how to handle errors in Enso in visual and textual work
- how to define visual widgets/red flags for graph

# Project structure

- imports, modules etc
- how to show Enso local library dependency 
- how to bring function to scope 

# Enso Libraries

- describe the most popular basic libraries in Enso with use examples like:
  - lists
  - Dataframes
  - Text

# More about type system and data types 

(not sure where it should go but I would say it is pretty advanced part)

- methods and functions in data type definition - differences

- data types as values 

- fields modifiers (lenses?)

- prisms 

- refinement types

- type inference

# Dataframes (?) Streams (?) Different data sources and combining (?)

This depends on the target. Idea - How to work with streams, what can you do with data frames - first example how to build some more complicated flow with dashboard?