"
Class: SCCP
                                                                                                    

This class implements optimizations for Druid.
It provides constant folding and constant propagation and dead code elimination using SCCP algorithm.

"
Class {
	#name : #SCCP,
	#superclass : #Object,
	#instVars : [
		'latticeValues',
		'CFGWorkList',
		'SSAWorkList',
		'CFGWorkDict'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
SCCP >> applyTo: cfg [
	
	| e |
	
	CFGWorkList := OrderedCollection new: cfg initialBasicBlock successors.  
	CFGWorkDict := Dictionary new. 
	
	CFGWorkList do: [ :edge | CFGWorkDict at: ( cfg initialBasicBlock, edge) put: 'Unexecuted' ]. 
	SSAWorkList := OrderedCollection new. 
	
	cfg blocksDo: [ :b | b instructionsDo: [ :i | latticeValues at: i put: 'Top'] ].
	
	[CFGWorkList notEmpty or: SSAWorkList notEmpty] whileTrue: [ 
		CFGWorkList notEmpty ifTrue: [ 
			 e := CFGWorkList removeFirst. 
			 (CFGWorkDict valueAt: e = 'Unexecuted') ifTrue: [ 
				CFGWorkList at: e put: 'Executed'. . ]]].
]

{ #category : #operations }
SCCP >> dependentInstructionsOf: aRegister inList: aList [

	^ aList select: [ :anInstruction | (anInstruction operands collect: [ :op | op result ] ) includes: aRegister ]
]

{ #category : #operations }
SCCP >> getOperationList: aBlock regardingName: aRegister [
	| operationList |
	
	operationList := OrderedCollection new. 
	aBlock instructionsDo: [ :e | ((e operands collect: [ :op | op name ] ) includes: aRegister) ifTrue: [operationList add: e]]. 
	^ operationList 
]

{ #category : #operations }
SCCP >> initialize [

	super initialize.
	latticeValues := Dictionary new.
]

{ #category : #operations }
SCCP >> initializeLatticeValues: aBlock [ 

	aBlock instructions do: [ :i | latticeValues at: i put: 'Top']
]

{ #category : #operations }
SCCP >> interpreterOverLatticeValue: operation withInstruction: anInstruction onblock: aBlock [

	| operandCollection|
	(operation operand1 = anInstruction) ifTrue: [ 
	
	operandCollection := OrderedCollection new. 
	operandCollection add: anInstruction operand1. 
	
	operation operands: operandCollection]. 

	operation isPhiFunction ifTrue: [ 
		operation operand1 value = operation operand2 value ifTrue: [ 
			 ] ].
	

]

{ #category : #operations }
SCCP >> optimizeBlock: aBlock [

	| workList currentOperationProcessed operationList t|

	"Initialization"
	
	workList := OrderedCollection new. 

	aBlock instructionsDo: [ :e | e isPhiFunction ifTrue: [ latticeValues at: e put: 'Top'  ]].
	aBlock instructionsDo: [ :e | e className = 'DRCopy' ifTrue: [ latticeValues at: e put: (e operand1) ] ].
	aBlock instructionsDo: [ :e | e operand1 isRegister ifTrue: [ latticeValues at: e put: 'Bot' ] ].
	
	latticeValues keysDo: [ :key | ((self value: key) ~= 'Top') ifTrue: [ workList add: key]].
	[ workList isNotEmpty ] whileTrue: [ 
		currentOperationProcessed := workList removeFirst. 		
		operationList := self dependentInstructionsOf: currentOperationProcessed result inList: workList. 
		operationList do: [ :op | ((self value: op) ~= 'Bot') ifTrue: [ 
			t := (self value: op). 
			self interpreterOverLatticeValue: op withInstruction: currentOperationProcessed onblock: aBlock. 
			((self value: op) ~= t) ifTrue: [ 
				workList add: op. ]]]].
]

{ #category : #operations }
SCCP >> value: aRegister [

	"Return the lattice value for the register"
	^ latticeValues at: aRegister
]
