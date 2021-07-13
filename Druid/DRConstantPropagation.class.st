"
This class implements optimizations for Druid. 
It provides constant folding and constant propagation. 

Implementation :
It used the SCPP algorithm (source: http://www.r-5.org/files/books/computers/compilers/writing/Keith_Cooper_Linda_Torczon-Engineering_a_Compiler-EN.pdf). This algorithm used lattices. In order not to implement a lattice class, we replaced it by a dictionnary named latticesValues. The 'top' and 'bottom' values were replaces respectively by 'true' and 'false' objects. 
"
Class {
	#name : #DRConstantPropagation,
	#superclass : #Object,
	#instVars : [
		'latticeValues'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRConstantPropagation >> applyTo: cfg [
	
	cfg blocksDo: [ :b | self initializeLatticeValues: b ].
	cfg blocksDo: [ :b | self optimizeBlock: b ].
	cfg instructionsDo: [ :e | (e isPhiFunction) ifTrue: [ self interpretePhiFunction: e ] ]. 
]

{ #category : #operations }
DRConstantPropagation >> initialize [

	super initialize.
	latticeValues := Dictionary new.
]

{ #category : #operations }
DRConstantPropagation >> initializeLatticeValues: aBlock [ 

	aBlock instructions do: [ :i | latticeValues at: i put: 'Top']
]

{ #category : #operations }
DRConstantPropagation >> interpretePhiFunction: operation [

	| operandsList |
	
	operation isPhiFunction ifTrue: [ 
		operation operand1 operand1 value = operation operand2 operand1 value ifTrue: [ 
			operandsList := OrderedCollection new. 
			operandsList add: operation operand1 operand1. 
			operation operands: operandsList]].
]

{ #category : #operations }
DRConstantPropagation >> interpreterOverLatticeValue: operation withInstruction: anInstruction onblock: aBlock [

	| operandCollection |
	operation operand1 = anInstruction ifFalse: [ ^ self ].

	operandCollection := OrderedCollection new.
	operandCollection add: anInstruction operand1.
	operation operands: operandCollection
]

{ #category : #operations }
DRConstantPropagation >> operationListOf: aRegister withList: aList [

	^ aList select: [ :anInstruction | (anInstruction operands collect: [ :op | op result ] ) includes: aRegister ]
]

{ #category : #operations }
DRConstantPropagation >> optimizeBlock: aBlock [

	| workList currentOperationProcessed operationList t|

	"Initialization"
	
	workList := OrderedCollection new. 

	aBlock instructionsDo: [ :e | e isCopy ifTrue: [ latticeValues at: e put: e operand1 ] ].
	
	latticeValues keysDo: [ :key | ((self value: key) ~= 'Top') ifTrue: [ workList add: key]].
	[ workList isNotEmpty ] whileTrue: [ 
		currentOperationProcessed := workList removeFirst. 		
		operationList := self operationListOf: currentOperationProcessed result withList: aBlock instructions. 
		operationList do: [ :op | ((self value: op) ~= 'Bot') ifTrue: [ 
			t := self value: op. 
			self interpreterOverLatticeValue: op withInstruction: currentOperationProcessed onblock: aBlock. 
			((self value: op) ~= t) ifTrue: [ 
				workList add: op. ]]]].
]

{ #category : #operations }
DRConstantPropagation >> value: aRegister [

	"Return the lattice value for the register"
	^ latticeValues at: aRegister
]
