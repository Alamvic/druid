"
This class is an available optimisations for Druid.

It is called on DRDeadCodeElimination.

It removes the useless operations. The useless operations are the operations that are not called in a return, a load or a store. All the dependencies of those operations are also preserved. 

All the dependencies of the main operations are kept in an intermediate data structures, and are checked before being removed or not. 

In this algorithm, the jumps are considered as classic operations. 


"
Class {
	#name : #DRDeadCodeElimination,
	#superclass : #Object,
	#instVars : [
		'instancecfg',
		'toKeepList',
		'workList'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRDeadCodeElimination >> applyTo: cfg [

	instancecfg := cfg.
	self markInitialInstructions: cfg.

	[ workList isEmpty ] whileFalse: [ 
		| instruction |
		instruction := workList removeFirst.
		toKeepList add: instruction.
		workList addAll: (instruction basicBlock predecessors collect: [ :b | b endInstruction ]). 
		workList addAll: (self dependenciesOf: instruction) ].
	self sweepInstructions: cfg
]

{ #category : #operations }
DRDeadCodeElimination >> dependenciesOf: anInstruction [
	
	^ anInstruction operands select: [ :op | op isInstruction ]
]

{ #category : #operations }
DRDeadCodeElimination >> initialize [ 

	toKeepList := OrderedCollection new. 
	workList := OrderedCollection new. 
]

{ #category : #operations }
DRDeadCodeElimination >> markInitialInstructions: cfg [

	cfg instructionsDo: [ :i | i isValidEndInstruction ifTrue: [ workList add: i] ]
]

{ #category : #operations }
DRDeadCodeElimination >> optimiseBlock: aBlock [ 

	"self removeDuplicatedInstructions: aBlock. "
	self removeDeadEndInstructions: aBlock. 
	
]

{ #category : #operations }
DRDeadCodeElimination >> removeDeadEndInstructions: aBlock [

	| dependencies |

	aBlock instructionsDo: [ :i | 
		(i isValidEndInstruction) ifFalse: [
			dependencies := self dependenciesOf: i. 
			dependencies isEmpty ifTrue: [ 
				 removeList add: i.
				 i operands do: [:o | o isConstant ifFalse: [removeList add: o]]]]].
]

{ #category : #operations }
DRDeadCodeElimination >> removeDuplicatedInstructions: aBlock [

	|instructionList currentInstruction|
	
	instructionList := aBlock instructions. 
	
	[instructionList isEmpty] whileFalse: [ 
		 currentInstruction := instructionList removeFirst. 
		 instructionList do: [:i | 
			currentInstruction = i ifTrue: [
				removeList add: i ]]]. 
]

{ #category : #operations }
DRDeadCodeElimination >> sweepInstructions: cfg [

	| toRemove |
	
	toRemove := OrderedCollection new. 
	
	cfg instructionsDo: [ :i | 
		(toKeepList includes: i) ifFalse: [ toRemove add: i ] ].
	
	toRemove do: [ :i | i removeFromCFG ]
]
