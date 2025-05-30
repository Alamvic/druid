"
I am a Druid optimisation that implements a mark-and-sweep like dead code elimination on IR instructions.

I am split in two phases:
 - I first recursively mark all instructions that are mandatory for the correct execution of the compiled unit. For example, I mark the instruction that sets the return value of the function, and all its (recursive) dependencies
 - Second, I iterate all instructions and remove those that are were not marked

I consider as initial mandatory instructions those instructions that have visible side effects: return and store instructions.
Then, any instruction that generates a value used by a mandatory instruction is also mandatory.

## Example

In the following example, the only initial mandatory instruction is the return instruction.
The Return instruction depends on the value generated by R2, and R2 depends on the value generated by R1.
All those three instructions are then marked as mandatory.
Finally, the instruction `R3 := 17` is discarded as it is not required by the execution.

```
R1 := 2
R2 := R1 + 7
R3 := 17
Return R2
```

## Implementation Details

I am implemented using a worklist algorithm as a queue.
A worklist contains the next instructions to treat.
Every time an instruction is processed, we mark it and put all its dependencies into the worklist.
We represent the marked instructions by putting them inside a marked collection.

Care is taken to mark correctly jump instructions.
When an instruction is marked, as we consider it a mandatory instruction, we should also consider all the jumps that target the current basic block as mandatory too.
This means that every time an instruction is marked, the last instruction of all predecessors of the current basic block are also added to the work list.
Since we consider jumps as normal instructions, this will trigger the marking of the entire path that reaches mandatory instructions.
"
Class {
	#name : 'DRDeadCodeElimination',
	#superclass : 'DROptimisation',
	#instVars : [
		'workList',
		'markedInstructions'
	],
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'operations' }
DRDeadCodeElimination >> applyTo: cfg [

	self initialize.
	self markInitialInstructions: cfg.
	self markInstructions.
	self sweepInstructions: cfg
]

{ #category : 'testing' }
DRDeadCodeElimination >> canBeAppliedIn: aDRControlFlowGraph [

	^ aDRControlFlowGraph instructions anySatisfy: [ :i | 
		  i isMandatoryInstruction not and: [ i users isEmpty ] ]
]

{ #category : 'operations' }
DRDeadCodeElimination >> dependenciesOf: anInstruction [
	
	^ anInstruction dataDependencies select: [ :op | op isInstruction ]
]

{ #category : 'operations' }
DRDeadCodeElimination >> initialize [ 

	markedInstructions := IdentitySet new. 
	workList := OrderedCollection new.
]

{ #category : 'operations' }
DRDeadCodeElimination >> markInitialInstructions: cfg [

	cfg instructionsDo: [ :i | i isMandatoryInstruction ifTrue: [ workList add: i] ]
]

{ #category : 'operations' }
DRDeadCodeElimination >> markInstructions [

	[ workList isEmpty ] whileFalse: [ 
		| instruction |
		instruction := workList removeFirst.
		"Mark the instruction"
		(markedInstructions includes: instruction) ifFalse: [
			markedInstructions add: instruction.
			
			"Consider the jump instructions that must be executed to arrive to this instruction"
			workList addAll:
				(instruction basicBlock predecessors collect: [ :b | 
					 b endInstruction ]).
			
			"Consider for marking all dependencies of the current instruction"
			workList addAll: (self dependenciesOf: instruction) ] ]
]

{ #category : 'operations' }
DRDeadCodeElimination >> sweepInstructions: cfg [

	"Remove all non marked instructions from the CFG.
	Use a temporary list to collect all instructions to remove, to avoid removing things from a collection while we iterate it"
	| toRemove |
	
	toRemove := OrderedCollection new. 

	cfg instructionsDo: [ :i | 
		(markedInstructions includes: i) ifFalse: [ toRemove add: i ] ].
	
	toRemove do: [ :i | i removeFromCFG ]
]
