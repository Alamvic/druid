"
This class implements constant propagation and constant folding for Druid. 
These optimisations work using the SCCP algorithm. 

More information about the SCPP algoithm : 
http://www.r-5.org/files/books/computers/compilers/writing/Keith_Cooper_Linda_Torczon-Engineering_a_Compiler-EN.pdf
Section 9.3 
"
Class {
	#name : #DRSCCPConstantPropagation,
	#superclass : #Object,
	#instVars : [
		'latticeValues'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRSCCPConstantPropagation >> applyTo: cfg [
	
	| workList currentOperationProcessed operationList t|
	cfg blocksDo: [ :b | self initializeLatticeValues: b ].
	
	"Initialization"
	
	cfg blocksDo: [ :b | b instructionsDo: [ :i | i isJump ifFalse: [ b replaceInstruction: i by: (i tryConstantFolding)]]].
	
	workList := OrderedCollection new. 
	latticeValues keysDo: [ :key | ((self value: key) ~= 'Top') ifTrue: [ workList add: key]].
	[ workList isNotEmpty ] whileTrue: [ 
		currentOperationProcessed := workList removeFirst. 		
		operationList := self dependentInstructionsOf: currentOperationProcessed inList: cfg instructions.
		operationList do: [ :op | ((self value: op) ~= 'Bot') ifTrue: [ 
			t := self value: op. 
			"self interpretOverLatticeValue: op withInstruction: currentOperationProcessed. "
			self interpretOverLatticeValue2: op. 
			((self value: op) ~= t) ifTrue: [ 
				cfg blocksDo: [ :b | b instructionsDo: [ :i | i isJump ifFalse: [ b replaceInstruction: i by: (i tryConstantFolding)]]].
				workList add: op.]]]]
]

{ #category : #operations }
DRSCCPConstantPropagation >> dependentInstructionsOf: aValue inList: aList [

	^ aList select: [ :anInstruction | anInstruction operands includes: aValue ]
]

{ #category : #operations }
DRSCCPConstantPropagation >> initialize [

	super initialize.
	latticeValues := Dictionary new.
]

{ #category : #operations }
DRSCCPConstantPropagation >> initializeLatticeValues: aBlock [ 

	aBlock instructionsDo: [ :i | | value | 
		value := (i isCopy and: [i operand1 isConstant])
			ifTrue: [ i operand1 ]
			ifFalse: [ (i isLoad or: [i isStore]) 
							ifTrue: ['Bot']
							ifFalse: ['Top']].
		latticeValues at: i put: value.
	 ].

]

{ #category : #operations }
DRSCCPConstantPropagation >> interpretOverLatticeValue2: operation [ 

	operation isPhiFunction ifTrue: [ self interpretePhiFunction: operation. ^ self. ]. 
	operation operands: (operation operands collect: [ :op | self interpreteValue: op ]). 
	"Add constant folding here"

	
]

{ #category : #operations }
DRSCCPConstantPropagation >> interpretePhiFunction: operation [

	| operandsList |
	operation operand1 operand1 value = operation operand2 operand1 value 
		ifTrue: [ 
			operandsList := OrderedCollection new.
			operandsList add: operation operand1 operand1.
			operation operands: operandsList.
			latticeValues at: operation put: operation operand1. ] 
]

{ #category : #operations }
DRSCCPConstantPropagation >> interpreteValue: op [

	|lv|
	
	lv := latticeValues at: op ifAbsent: [ op ]. 
	(('Top', 'Bot') includes: lv) ifTrue: [ ^ op ]. 
	^ lv. 
]

{ #category : #operations }
DRSCCPConstantPropagation >> value: aRegister [

	"Return the lattice value for the register"
	^ latticeValues at: aRegister
]
