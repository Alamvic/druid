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
DRConstantPropagation >> applyTo: anIR [
	
	anIR blocksDo: [ :b | self optimizeBlock: b ]
]

{ #category : #operations }
DRConstantPropagation >> getOperationList: aBlock regardingName: aRegister [
	| operationList |
	
	operationList := OrderedCollection new. 
	aBlock instructionsDo: [ :e | ((e operands collect: [ :op | op name ] ) includes: aRegister) ifTrue: [operationList add: e]]. 
	^ operationList 
]

{ #category : #operations }
DRConstantPropagation >> initialize [

	super initialize.
	latticeValues := Dictionary new.
]

{ #category : #operations }
DRConstantPropagation >> interpreterOverLatticeValue: operation withInstruction: anInstruction onblock: aBlock [

	| operandCollection|
	"self halt. "
	(operation operand1 name = anInstruction result name) ifFalse: [ ^ self ].
	
	operandCollection := OrderedCollection new. 
	operandCollection add: anInstruction operand1. 
	
	operation operands: operandCollection. 
	
]

{ #category : #operations }
DRConstantPropagation >> operationList: aBlock usingRegister: aRegister withList: aList [
	
	^ aList select: [ :anInstruction | (anInstruction operands collect: [ :op | op result ] ) includes: aRegister ]
]

{ #category : #operations }
DRConstantPropagation >> operationListOf: aRegister withList: aList [
	self halt. 
	^ aList select: [ :anInstruction | (anInstruction operands collect: [ :op | op result ] ) includes: aRegister ]
]

{ #category : #operations }
DRConstantPropagation >> optimizeBlock: aBlock [

	| workList currentOperationProcessed operationList t|

	"Initialization"
	
	workList := OrderedCollection new. 

	aBlock instructionsDo: [ :e | e isPhiFunction ifTrue: [ latticeValues at: e put: 'Top'  ]].
	aBlock instructionsDo: [ :e | e className = 'DRCopy' ifTrue: [ latticeValues at: e put: (e operand1) ] ].
	aBlock instructionsDo: [ :e | e operand1 isRegister ifTrue: [ latticeValues at: e put: 'Bot' ] ].
	
	latticeValues keysDo: [ :key | ((self value: key) ~= 'Top') ifTrue: [ workList add: key]].
	self halt. 
	[ workList isNotEmpty ] whileTrue: [ 
		currentOperationProcessed := workList removeFirst. 		
		operationList := self operationListOf: currentOperationProcessed result withList: workList. 
		operationList do: [ :op | ((self value: op) ~= 'Bot') ifTrue: [ 
			t := (self value: op). 
			self interpreterOverLatticeValue: op withInstruction: currentOperationProcessed  onblock: aBlock. 
			((self value: op) ~= t) ifTrue: [ 
				workList add: op. ]]]].
			
	aBlock instructionsDo: [ :e | e tryConstantFolding ]. 
]

{ #category : #operations }
DRConstantPropagation >> value: aRegister [

	"Return the lattice value for the register"
	^ latticeValues at: aRegister
]
