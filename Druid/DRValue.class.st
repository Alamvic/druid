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

{ #category : #'users-definitions' }
DRValue >> addUser: anInstruction [
	
	"Nothing"
]

{ #category : #converting }
DRValue >> asDRInterpreterValue [

	^ self
]

{ #category : #convering }
DRValue >> asDRValue [
	
	^ self
]

{ #category : #accessing }
DRValue >> isConditionalJump [ 

	^ false
]

{ #category : #testing }
DRValue >> isConstant [
	
	^ false
]

{ #category : #testing }
DRValue >> isCopy [ 
	
	^ false
]

{ #category : #testing }
DRValue >> isDRValue [
	
	^ true
]

{ #category : #testing }
DRValue >> isInstruction [

	^ false
]

{ #category : #accessing }
DRValue >> isJump [ 

	^ false
]

{ #category : #visiting }
DRValue >> isLoad [ 

	^ false
]

{ #category : #accessing }
DRValue >> isNoop [ 
	
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

{ #category : #visiting }
DRValue >> isStore [

	^ false 
]

{ #category : #visiting }
DRValue >> isValidEndInstruction [

	^ false
]

{ #category : #testing }
DRValue >> isVirtualRegister [
	
	^ false
]

{ #category : #joining }
DRValue >> joinNonNull: anotherValue onIRGenerator: aBuilder from: originBlocks [

	^ (DRPhiFunction joinedVariables: { anotherValue. self })
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

{ #category : #visiting }
DRValue >> removeFromCFG [

	block removeInstruction: self. 
]

{ #category : #'users-definitions' }
DRValue >> removeUser: anInstruction [
	
	"Nothing"
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
