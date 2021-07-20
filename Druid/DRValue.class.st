Class {
	#name : #DRValue,
	#superclass : #Object,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRValue >> acceptVisitor: aVisitor [

	self subclassResponsibility
]

{ #category : #building }
DRValue >> addToBlock: aDRBasicBlock [ 
	
	aDRBasicBlock addInstruction: self
]

{ #category : #converting }
DRValue >> asDRInterpreterValue [

	^ self
]

{ #category : #testing }
DRValue >> isConstant [
	
	^ false
]

{ #category : #testing }
DRValue >> isInstruction [

	^ false
]

{ #category : #testing }
DRValue >> isPhiFunction [

	^ false
]

{ #category : #testing }
DRValue >> isRegister [

	^ false
]

{ #category : #testing }
DRValue >> isVirtualRegister [
	
	^ false
]

{ #category : #joining }
DRValue >> joinNonNull: anotherValue onIRGenerator: aBuilder from: originBlocks [

	| copies |
	copies := { anotherValue. self } with: originBlocks collect: [ :v :b |
		b addInstruction: (aBuilder instantiate: DRCopy operands: { v }).
	].
	^ (DRPhiFunction joinedVariables: copies)
		result: aBuilder allocateTemporaryRegister;
		originBlocks: originBlocks;
		addToBlock: aBuilder currentBasicBlock;
		origin: thisContext stack;
		yourself
]

{ #category : #joining }
DRValue >> joinValue: anotherValue onIRGenerator: anIRGenerator from: originBlocks [

	self == anotherValue ifTrue: [ ^ self ].
	^ anotherValue joinNonNull: self onIRGenerator: anIRGenerator from: originBlocks
]

{ #category : #interpretation }
DRValue >> lookupClass [
	
	self subclassResponsibility
]

{ #category : #accessing }
DRValue >> result [

	^ self
]

{ #category : #accessing }
DRValue >> singleChild [
	
	^ self
]

{ #category : #accessing }
DRValue >> targets [

	^ #()
]

{ #category : #accessing }
DRValue >> tryConstantFolding [

	^ self
]
