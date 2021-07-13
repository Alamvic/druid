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
		'SSAWorkList'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
SCCP >> applyTo: cfg [
	
	CFGWorkList := Dictionary new: cfg initialBasicBlock successors.  
	SSAWorkList := OrderedCollection new. 
	
	CFGWorkList do: [ :edge | edge value: 'Unexecuted']. 
	
	cfg blocksDo: [ :b | b instructionsDo: [ :i | latticeValues at: i put: 'Top'] ].
	
	cfg blocksDo: [ :b | self initializeLatticeValues: b ].
	cfg blocksDo: [ :b | self optimizeBlock: b ]
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
SCCP >> operationListOf: aRegister withList: aList [

	^ aList select: [ :anInstruction | (anInstruction operands collect: [ :op | op result ] ) includes: aRegister ]
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
		operationList := self operationListOf: currentOperationProcessed result withList: workList. 
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
