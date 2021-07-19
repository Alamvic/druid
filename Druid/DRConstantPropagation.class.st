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
	
	| workList currentOperationProcessed operationList t|
	cfg blocksDo: [ :b | self initializeLatticeValues: b ].
	
	"Initialization"
	
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
				workList add: op.]]]]
]

{ #category : #operations }
DRConstantPropagation >> dependentInstructionsOf: aValue inList: aList [

	^ aList select: [ :anInstruction | anInstruction operands includes: aValue ]
]

{ #category : #operations }
DRConstantPropagation >> initialize [

	super initialize.
	latticeValues := Dictionary new.
]

{ #category : #operations }
DRConstantPropagation >> initializeLatticeValues: aBlock [ 

	aBlock instructionsDo: [ :i | | value | 
		value := (i isCopy and: [i operand1 isConstant])
			ifTrue: [ i operand1 ]
			ifFalse: [ 'Top' ].
		latticeValues at: i put: value.
	 ].

]

{ #category : #operations }
DRConstantPropagation >> interpretOverLatticeValue2: operation [ 

	operation isPhiFunction ifTrue: [ self interpretePhiFunction: operation. ^ self. ]. 
	operation operands: (operation operands collect: [ :op | self interpreteValue: op ]). 

	
]

{ #category : #operations }
DRConstantPropagation >> interpretePhiFunction: operation [

	| operandsList |
	operation operand1 operand1 value = operation operand2 operand1 value 
		ifTrue: [ 
			operandsList := OrderedCollection new.
			operandsList add: operation operand1 operand1.
			operation operands: operandsList.
			latticeValues at: operation put: operation operand1. ]
]

{ #category : #operations }
DRConstantPropagation >> interpreteValue: op [

	|lv|
	
	lv := latticeValues at: op ifAbsent: [ op ]. 
	(('Top', 'Bot') includes: lv) ifTrue: [ ^ op ]. 
	^ lv. 
]

{ #category : #operations }
DRConstantPropagation >> value: aRegister [

	"Return the lattice value for the register"
	^ latticeValues at: aRegister
]
