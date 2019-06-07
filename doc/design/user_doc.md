# Enso Introduction

Simplicity and correctness

## Do we need a new programming language?

## Software correctness

## "Immediate connection to the data" 

Brett Victor blogpost introduction

## Simplicity. The Ultimate Sophistication

# Visual and Textual Representation

## Encoding



## Layout

Dedicated IDE for Enso is always providing autolayouting tools. There is no strict rules about graph shape or layout since this is artistical expression for the process but for textual representation we should name a few.

## Naming Rules

# Type System

A *type system* is a set of rules that assigns a property called *type* to the various constructs of a computer program, such as variables, expressions, functions or modules [link do wikipedi].

Enso is a statically typed language. 

Basic types and datatypes - just first informations

## Type Signatures

In Enso it is not needed to provide type signatures but it is 

# Data Types

## type construcotrs

The easiest way to create a new type is to use a constructor

## algebraic data types

An **algebraic data type** is a **data type** defined out of a combination of two constructions: products and sums. A product is a way to combine multiple values of different **types** into one.  

### Syntax sugar





## data types as values

## fields modifiers (lenses?)

## prisms 



# Basic Operators

## How to define your custom operator

## More about operators - precedence

# Pattern Matching

# Case and If

# Variables

Variables in statically typed language are different than in imperative. immutable memory etc.

## How you bring variables to scope

pattern matching as an only way to bring variable to scope

# Functions

## Creating and Using Functions

Functions are defined in a similar way to variables. In node editor you just write function body as an expression. The only difference is that in textual representation the function name is followed by parameters seperated by spaces.

## Function Type

As the function definition translates under the hood to an ordinary variable assignment, you can use the type expression to provide the compiler with an additional information about arguments and the result type. 

## Arguments

### Named Arguments

### Default arguments

## First class functions

## Partial function application 

## Code Blocks

## Uniform Calling Syntax (UCS)

# Recursion

# Lazy/strict

# Visualizations



# Dataframes (?) Streams (?) Different data sources and combining (?)

This depends on the target. Idea - How to work with streams, what can you do with data frames 

# Project structure (to be named better)

imports, modules etc

# Errors handling

# Enso Libraries