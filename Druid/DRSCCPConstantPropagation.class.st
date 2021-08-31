"
This class is an available optimisations for Druid.

It is called on DRControlFlowGraph.

It executes folding and propagation on the operations when they contain constants, which reduces the number of useful code lines and allows dead code elimination.

This is a first version of SCCP that doesn't take the conditionnal jumps into account, but efficiently provides propagation and folding. 

[Note] : This class is a step in the building of DRSCCP and is not meant to be called on use cases.

More information about the SCPP algorithm : 
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
			self interpretOverLatticeValue: op. 
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
	latticeValues := OrderedDictionary new.
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
DRSCCPConstantPropagation >> interpretOverLatticeValue: operation [ 

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