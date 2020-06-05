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

Druid works for now on Pharo9.0 (and very probably 8.0 too)

- Open a fresh Pharo9.0 image (recommended, use the launcher)
- Clone this repository using Iceberg
- Open the packages view of the repository
- Right click on BaselineOfDruid => install default

This will start the installation process, clone the dependencies, and load all necessary Pharo code.

You should also install the libraries unicorn and llvm.
If you are on linux, use your preferred package manager.
If you're on OSX, we recommend using homebrew.
In windows, we should check.

### (Prelude) An overview of the Pharo VM

The Pharo VM is a program that executes Pharo programs.
Its main components are execution engine, and a memory manager.
The execution engine is made of an interpreter (a bytecode interpreter), and a JIT compiler that compiles to machine code methods that are used often.
The memory manager implements how objects are stored in memory, and an automatic reclamation of memory (usually called garbage collector).

The Pharo VM is implemented in a subset Pharo itself called Slang.
Thus, we can use Pharo tools to execute, test and debug the VM in a "simulated" manner.
To produce a productive VM the Slang Pharo code is transpiled to C and then compiled in the current architecture.

Druid works on the Slang part of the VM, and its scope does not touch (for now) the C-code generation.

### (Prelude) Understanding the bytecode interpreter

Pharo methods are written as bytecodes and primitives.
For example, the following method is compiled as a method made of platform independent bytecode and an array of literals (also called literal frame) used in that method.
The bytecodes are virtual machine code instructions, the literals are the objects that represent fixed values used in that method.
For example, 1 and 17 are literals in this method. Besides numbers, other kind of literals are `'strings'`, literal arrays such as `#(a b c 1)` and characters `$A`. 

```smalltalk
MyClass >> foo
  ^ 1 + 17
```

We can inspect the method above `MyClass >> foo` and see its bytecodes and literals.
In the list below the first number is the bytecode index, the second is the instruction bytes, and then there is a mnemonic of the instruction and arguments.

```
25 <76> pushConstant: 1
26 <20> pushConstant: 17
27 <B0> send: +
28 <7C> returnTop
```

It is important to understant that the Pharo bytecode is based on a stack machine (in contrast with register machines).
This means that most of the data between operations is exchanged through a stack.
In the example above, the first bytecode pushes a 1 into the stack and the second bytecode pushes a 2.
Then, the third bytecode has to send a message `+`, so it pops two elements from the stack: one to use as the receiver, and one to use as an argument.
When the execution of the message send finishes, the result is pushed to the stack.
Finally, the last bytecode takes the top of the stack (the result of the addition), and returns it to the caller.

Each of these bytecodes is implemented in the VM as routines/methods.
The class implementing the bytecode interpreter is `StackInterpreter`.
For example, the `pushConstantOneBytecode` is the routine that pushes a 1 to the stack calling the `internalPush:` method.
Since pushing the value 1 is a very common operation, a special bytecode is used for it to avoid putting the 1 in the literal frame.

```smalltalk
StackInterpreter >> pushConstantOneBytecode

	self fetchNextBytecode.
	self internalPush: ConstOne.
```

The method `pushLiteralConstantBytecode` pushes a generic literal value to the stack also using `internalPush:`.
The value pushed is taken from the literal frame of the method, and the index is calculated from manipulating the `currentBytecode` variable.
Bytecode 33 pushes the first literal in the frame (33 bitAnd: 16r1F => 1), bytecode 34 pushes the second literal, and so on...

```smalltalk
StackInterpreter >> pushLiteralConstantBytecode
	<expandCases>
	self
		cCode: "this bytecode will be expanded so that refs to currentBytecode below will be constant"
			[self fetchNextBytecode.
			 self pushLiteralConstant: (currentBytecode bitAnd: 16r1F)]
		inSmalltalk: "Interpreter version has fetchNextBytecode out of order"
			[self pushLiteralConstant: (currentBytecode bitAnd: 16r1F).
			 self fetchNextBytecode]
```

Finally, bytecodes such as the message send `+` are implemented as follows.
First this bytecode gets the top 2 values from the stack.
Then it checks if boths are integers, and if the result is an integer, in which case it pushes the value and finishes.
If they are not integers, it tries to add them as floats.
If that fails, it will perform a (slow) message send using the `normalSend` method.

```smalltalk
StackInterpreter >> bytecodePrimAdd
	| rcvr arg result |
	rcvr := self internalStackValue: 1.
	arg := self internalStackValue: 0.
	(objectMemory areIntegers: rcvr and: arg)
		ifTrue: [result := (objectMemory integerValueOf: rcvr) + (objectMemory integerValueOf: arg).
				(objectMemory isIntegerValue: result) ifTrue:
					[self internalPop: 2 thenPush: (objectMemory integerObjectOf: result).
					^ self fetchNextBytecode "success"]]
		ifFalse: [self initPrimCall.
				self externalizeIPandSP.
				self primitiveFloatAdd: rcvr toArg: arg.
				self internalizeIPandSP.
				self successful ifTrue: [^ self fetchNextBytecode "success"]].

	messageSelector := self specialSelector: 0.
	argumentCount := 1.
	self normalSend
```

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