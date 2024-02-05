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

### Installing Dependencies

- You should also install the libraries unicorn and llvm.
- If you are on linux, use your preferred package manager.
- If you're on OSX, we recommend using homebrew.
- In windows, we should check.

#### Install Unicorn library for Pharo

```bash
# Unicorn 2 Setup for PharoVM
# Instructions at: https://github.com/pharo-project/unicorn/blob/pharo-vm-unicorn2/docs/COMPILE.md

git clone https://github.com/pharo-project/unicorn.git
cd unicorn
git checkout pharo-vm-unicorn2
mkdir build; cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make
```

Now replace possible already installed unicorn with the new compiled one, for example:
```bash
locate libunicorn.2.dylib
mv /opt/homebrew/Cellar/unicorn/2.0.1/lib/libunicorn.2.dylib /opt/homebrew/Cellar/unicorn/2.0.1/lib/libunicorn.2.old
cp libunicorn.2.dylib /opt/homebrew/Cellar/unicorn/2.0.1/lib
```

Open the Pharo image with the [Unicorn project loaded](https://github.com/pharo-project/pharo-unicorn) and check:
```smalltalk
UnicornLibrary uniqueInstance macModuleName 
```

### Installing Druid

Druid works for now on Pharo 11:

```smalltalk
EpMonitor disableDuring: [ 
	Metacello new
		repository: 'github://Alamvic/druid:main';
		baseline: 'Druid';
		load ]
```

This will start the installation process, clone the dependencies, and load all necessary Pharo code.

### (Prelude) An overview of the Pharo VM

The Pharo VM is a program that executes Pharo programs.
Its main components are execution engine, and a memory manager.
The execution engine is made of an interpreter (a bytecode interpreter), and a JIT compiler that compiles to machine code methods that are used often.
The memory manager implements how objects are stored in memory, and an automatic reclamation of memory (usually called garbage collector).

The Pharo VM is implemented in a subset Pharo itself called Slang.
Thus, we can use Pharo tools to execute, test and debug the VM in a "simulated" manner.
To produce a productive VM the Slang Pharo code is transpiled to C and then compiled in the current architecture.

Druid works on the Slang part of the VM, and its scope does not touch (for now) the C-code generation.

### (Prelude) Bytecodes and a stack machine

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

### (Prelude) Understanding the bytecode interpreter

When a bytecode method is executed by the interpreter, it iterates all bytecodes of a method and executes a VM routine for each of them.
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

When a bytecode method is executed a couple of times, the Pharo virtual machine decides to compile it to machine code.
Compiling the method to machine code avoids performance overhead due to instruction fetching, and allows one to perform several optimizations.
The compilation of a machine code method goes pretty similar to the interpretation of a method.
The JIT compiler iterates the bytecode method and for each of the bytecodes it executes a code generation routine.
This means that we will (almost) have a counterpart for each of the VM methods implementing bytecode interpretation.

For example, the machine code generator implemented for `StackInterpreter>>pushLiteralConstantBytecode` is `Cogit>>genPushLiteralConstantBytecode`.

```smalltalk
Cogit >> genPushLiteralConstantBytecode
	^self genPushLiteralIndex: (byte0 bitAnd: 31)

StackToRegisterMappingCogit >> genPushLiteralIndex: literalIndex "<SmallInteger>"
	"Override to avoid the BytecodeSetHasDirectedSuperSend check, which is unnecessary
	 here given the simulation stack."
	<inline: false>
	| literal |
	literal := self getLiteral: literalIndex.
	^self genPushLiteral: literal
```

The JIT'ted version of the addition bytecode (`genSpecialSelectorArithmetic`) is slightly more complicated, but it pretty much matches what it is done in the bytecode.

### Overview of Druid

In Druid, a meta-interpreter analyzes the bytecode interpreter code and generates an intermediate representation from it.
A compiler interface then generates machine code from the intermediate representation.
The output of the intermediate representation should have in general terms the same behaviour as the existing Cogit JIT compiler.

To verify the correctness of the compiler we use:
 - a machine code simulator (Unicorn)
 - a disassembler (llvm)

(please see the links to these projects in the references)

## The (meta-)interpreter

The setup is the following: we have one Pharo AST interpreter that we call the meta-interpreter that executes the code code of the 
`StackInterpreter` and generates the corresponding intermediate representation of `StackInterpreter` methods.
Check `Fun with interpreters` from the references to see more details on what an ASTs and abstract interpreters are.
In the code below, the meta-interpreter is called `DRASTInterpreter` (for the DruidAST interpreter) and it will analyse the methods 
of the stack interpreter returned byt the expression `Druid new newBytecodeInterpreter`.
For this task, the meta-interpreter uses an IR builder that is responsible for encapsulating the logic of IR building.

```smalltalk
builder := DRIRBuilder new.
builder isa: #X64.

astInterpreter := DRASTInterpreter new.
astInterpreter vmInterpreter: Druid new newBytecodeInterpreter.
astInterpreter irBuilder: builder.
```

This AST interpreter then receives as input a list of bytecodes to analyze, it maps each bytecode to the routine to execute, 
obtains the AST and interprets each of the instructions of the AST using a visitor pattern.

```smalltalk
astInterpreter interpretBytecode: #[76].
```

### Visiting the AST

The `DRASTInterpreter` class implements a `visiting` protocol where the `visit` methods are grouped.
Most of the visit methods are simple, like the following ones:

```smalltalk
DRASTInterpreter >> visitSelfNode: aRBSelfNode 

	^ self receiver

DRASTInterpreter >> visitTemporaryNode: aRBTemporaryNode 
	
	^ currentContext temporaryNamed: aRBTemporaryNode name
```

### Context reification

To properly analyze the scope of temporary variables, the AST interpret reifies the contexts/stack frames.
On each method or block activation, a new context is pushed to the stack.
The temporary variables are then read and written from the current context.
When a method or block returns, the current context is popped from the stack, to return to the caller context.

### Interpreting Message sends

The most important operation in a Pharo program are message sends.
Message sends are used not only for normal method invocations, but also for common operators such as additions (`+`) and multiplications (`*`) and control flow such as conditionals (`ifTrue:`) and loops (`whileTrue:`).
However, some of these special cases require special treatments when generating code.
For example, a multiplication should directly generate a normal multiplication and not require interpreting how that multiplication is implemented.

To manage such special cases, we keep a table in the interpreter that maps (special selector -> special interpretation).
When we find a message send in the AST, we lookup the selector in the table.
If we find a special case in the entry, we invoke that special entry in the interpreter.
Otherwise, we lookup the method and activate the new method, which will recursively continue the interpretation

```smalltalk
DRASTInterpreter >> visitMessageNode: aRBMessageNode 
	
	| arguments astToInterpret receiver |
	
	"First interpret the arguments to generate instructions for them.
	If this is a special selector, treat it specially with those arguments.
	Otherwise, lookup and interpret the called method propagating the arguments"
	receiver := aRBMessageNode receiver acceptVisitor: self.
	arguments := aRBMessageNode arguments collect: [ :e | e acceptVisitor: self ].

	specialSelectorTable
		at: aRBMessageNode selector
		ifPresent: [ :selfSelectorToInterpret |
			^ self perform: selfSelectorToInterpret with: aRBMessageNode with: receiver with: arguments ].

	astToInterpret := self
		lookupSelector: aRBMessageNode selector
		receiver: receiver
		isSuper: aRBMessageNode receiver isSuper.
	^ self interpretAST: astToInterpret withReceiver: receiver withArguments: arguments.
```

For example, the following code snippet shows how the `internalPush:` is mapped as just a normal push instruction in the intermediate representation.

```smalltalk
DRASTInterpreter >> initialize

	super initialize.
	irBuilder := DRIRBuilder new.
	
	specialSelectorTable := Dictionary new.
    ...
	specialSelectorTable at: #internalPush: put: #interpretInternalPushOn:receiver:arguments:.
    
DRASTInterpreter >> interpretInternalPushOn: aRBMessageNode receiver: aStackInterpreterSimulatorLSB arguments: aCollection 

    ^ irBuilder push: aCollection first
```

### The intermediate representation

The intermediate representation is generated by a builder object, instance of `DRIRBuilder`.
The AST interpreter collaborates with it to create instructions, new basic blocks and so on.
The intermediate representation that is created is somewhat inspired on a low-level intermediate representation as described in [Linear Scan Register Allocation for the Java HotSpot™ Client Compiler].

### Some meta-interpretation of special cases

Not all special cases generate instructions or interact with the DRIRBuilder.
Some of them actually modify the state of the AST interpreter.
For example, a special case is the `fetchNextBytecode` instruction, that makes the interpreter move to the next byte in the list of bytecodes.
To simulate the same behaviour in our interpreter, its special case is implemented as follows:

```smalltalk
DRASTInterpreter >> interpretFetchNextBytecodeOn: aMessageSendNode receiver: aReceiver arguments: arguments

	self fetchNextInstruction
    
DRASTInterpreter >> fetchNextInstruction

	currentBytecode := instructionStream next
```

### The compiler interface

Once the interpreter finishes its job, the irBuilder will contain the intermediate representation instructions.
We can then generate machine code from them using the `DRIntermediateRepresentationToMachineCodeTranslator` class.

```smalltalk
	astInterpreter irBuilder assignPhysicalRegisters.

	mcTranslator := DRIntermediateRepresentationToMachineCodeTranslator
		translate: builder instructions
		withCompiler: cogit.

	address := cogit methodZone freeStart.
	endAddress := mcTranslator generate.
```

`DRIntermediateRepresentationToMachineCodeTranslator` iterates all the instructions and uses the double-dispatch pattern to 
generate code for each of them.
It uses a backend to generate the actual machine code.

```smalltalk
aCollection do: [ :anIRInstruction | 
	anIRInstruction accept: self ].

DRIntermediateRepresentationToMachineCodeTranslator >> visitAdd:
DRIntermediateRepresentationToMachineCodeTranslator >> visitLoad:
DRIntermediateRepresentationToMachineCodeTranslator >> visitPush:
```

## About the Tests

The whole process has the following steps:

1. The bytecode interpreter is interpreted to generate the instructions in the IRBuilder
2. Then the physical registers will be allocated to the Druid instructions
3. The DRInstructions generates the AbstractInstructions (Cogit) that basically are one to one representations with the machine code
4. Compilation to machine code byte array

So, we have different tests to test different stages in the transformation:

- The class `DRIRBuilderTest` tests the construction of the ir.
- The class `DRASTInterpreterTest` tests the effect of interpreting ASTs
- The DRIntermediateRepresentationToMachineCodeTranslatorTest simulates that each DR instrucion is correctly translated to the corresponding machine code
- The class `DRSimulateGeneratedBytecodeTest` tests from end-to-end the generation of code of a bytecode and its execution in a machine code simulator

Other test classes test a basic register allocation algorithm (`DRRegisterAllocationTest`), the generation of machine code from an IR without passing through the AST (`DRIntermediateRepresentationToMachineCodeTranslatorTest`) and the execution of machine code from an IR without passing through the AST  (`DRSimulateGeneratedCodeTest`).

## Little exercises
- In tbe book [https://github.com/SquareBracketAssociates/PatternsOfDesign/releases](https://github.com/SquareBracketAssociates/PatternsOfDesign/releases)
- Chapter 4: Die and DieHandle double Dispatch (if you want to make sure that Double Dispatch has been understood do the Stone Paper Scissor Chapter)
- Chapter 3 A little expression interpreter
- Chapter 6 Understanding visitor 
- After reading [https://github.com/SquareBracketAssociates/Booklet-FunWithInterpreters](https://github.com/SquareBracketAssociates/Booklet-FunWithInterpreters)


## References
 - Linear Scan Register Allocation for the Java HotSpot™ Client Compiler
   http://www.ssw.uni-linz.ac.at/Research/Papers/Wimmer04Master/
 - Practical partial evaluation for high-performance dynamic language runtimes
   https://dl.acm.org/doi/10.1145/3062341.3062381
 - Structure and Interpretation of Computer Programs
   https://web.mit.edu/6.001/6.037/sicp.pdf
 - Fun with Interpreters
   https://github.com/SquareBracketAssociates/Booklet-FunWithInterpreters/releases/download/continuous/fun-with-interpreters-wip.pdf
 - [Trace-Based Register Allocation](https://gitlab.inria.fr/RMOD/vm-papers/-/blob/master/compilation+JIT/2016_Trace-based%20Register%20Allocation%20in%20a%20JIT%20Compiler.pdf)
 - Paper explaining the motivations behind Sista Bytecode
   https://github.com/SquareBracketAssociates/Booklet-PharoVirtualMachine/raw/master/bib/iwst2014_A%20bytecode%20set%20for%20adaptive%20optimizations.pdf

 - https://github.com/unicorn-engine/unicorn
 - https://github.com/guillep/pharo-unicorn
 - http://llvm.org/
 - https://github.com/guillep/pharo-llvmDisassembler
