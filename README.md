# Druid: Meta-interpretation for Just-In-Time compiler generation

The Druid project explores the automatic generation of machine code templates from bytecode interpreters using an abstract interpreter on the existing bytecode interpreter (a meta-interpreter).
This approach could benefit from having a single runtime implementation and having a JIT compiler generated from it.

## Context

JIT (Just-in-Time) compilers are an optimization technique often used for interpreted languages and virtual machines.
They allow to spend time optimizing only frequently used code, while falling back in slower execution engines for non-frequent code.
For example, the Pharo and the Java VM run on a bytecode interpreter and eventually compile machine code for methods that are frequently called.

Nowadays, the Pharo Virtual Machine is implemented in a subset of the Pharo language called Slang.
The Virtual Machine developers then benefit from the high-level tools used to work with Pharo code, such as the code editors, testing frameworks and debuggers.
In a later stage, the Virtual Machine code written in Slang is transpiled to C and then compiled to the target architectures.

The current Pharo JIT compiler that is part of the Virtual Machine, aka Cogit, implements an architecture based on templates of native code per bytecode.
When a method is compiled, each bytecode is mapped to its corresponding template. All templates are concatenated to form a single machine code method.
This architecture has as drawback that the behavior of the Pharo language is duplicated in both the bytecode interpreter and their corresponding machine code templates.

The Druid project explores the automatic generation of machine code templates from bytecode interpreters using an abstract interpreter on the existing bytecode interpreter (a meta-interpreter).

## Getting started

### Installing Druid

### (Prelude) Understanding the bytecode interpreter

### (Prelude) Understanding the existing Cogit JIT compiler

### Overview of Druid

A meta-interpreter analyzes the bytecode interpreter code and generates an intermediate representation from it.
A compiler interface then generates machine code from the intermediate representation.
The output of the intermediate representation should have the same behaviour as the existing Cogit JIT compiler.

To verify the correctness of the compiler we use:
 - a machine code simulator (Unicorn)
 - a disassembler (llvm)

## The (meta-)interpreter

## The compiler interface

## References
 - Linear Scan Register Allocation for the Java HotSpot™ Client Compiler
   http://www.ssw.uni-linz.ac.at/Research/Papers/Wimmer04Master/
 - Practical partial evaluation for high-performance dynamic language runtimes
   https://dl.acm.org/doi/10.1145/3062341.3062381
 - Structure and Interpretation of Computer Programs
   http://web.mit.edu/alexmv/6.037/sicp.pdf
 - Fun with Interpreters
   https://github.com/SquareBracketAssociates/Booklet-FunWithInterpreters/releases/download/continuous/fun-with-interpreters-wip.pdf

 - https://github.com/unicorn-engine/unicorn
 - https://github.com/guillep/pharo-unicorn
 - http://llvm.org/
 - https://github.com/guillep/pharo-llvmDisassembler