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

{ #category : #convering }
DRValue >> asDRValue [
	
	^ self
]

{ #category : #memory }
DRValue >> asMemoryAddress [
	
	self subclassResponsibility
]

{ #category : #copying }
DRValue >> copyToVariableIn: aDRIRGenerator inNode: aNode [
	
	^ aDRIRGenerator
		addInstructionFrom: aNode
		instructionKind: DRCopy
		operands: { self }.
]

{ #category : #accessing }
DRValue >> hasId: id [

	^ false
]

{ #category : #testing }
DRValue >> hasUser: anInstruction [
	
	"For values non-instructions we can say this always holds"
	^ true
]

{ #category : #testing }
DRValue >> isComparison [

	^ false
]

{ #category : #testing }
DRValue >> isComplexCondition [

	^ false
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
DRValue >> isDRBlockClosure [
	
	^ false
]

{ #category : #testing }
DRValue >> isGlobal [

	^ self isNullValue or: [ self isPhysicalRegister ]
]

{ #category : #testing }
DRValue >> isInstruction [

	^ false
]

{ #category : #accessing }
DRValue >> isJump [ 

	^ false
]

{ #category : #testing }
DRValue >> isLoad [
	
	^ false
]

{ #category : #visiting }
DRValue >> isMandatoryInstruction [

	^ false
]

{ #category : #testing }
DRValue >> isMemoryAddress [
	
	^ false
]

{ #category : #testing }
DRValue >> isNoResult [
	
	^ false
]

{ #category : #accessing }
DRValue >> isNoop [ 
	
	^ false
]

{ #category : #visiting }
DRValue >> isNullValue [

	^ false
]

{ #category : #testing }
DRValue >> isObjectReferenceValue [

	^ false
]

{ #category : #testing }
DRValue >> isPhiFunction [

	^ false
]

{ #category : #testing }
DRValue >> isPhysicalRegister [

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

{ #category : #testing }
DRValue >> isVirtualRegister [
	
	^ false
]

{ #category : #joining }
DRValue >> joinNonNull: anotherValue onIRGenerator: aBuilder from: originBlocks [

	^ (DRPhiFunction joinedVariables: { anotherValue. self })
		result: aBuilder allocateTemporaryRegister;
		addToBlock: aBuilder currentBasicBlock;
		origin: thisContext stack
]

{ #category : #joining }
DRValue >> joinValue: anotherValue onIRGenerator: anIRGenerator from: originBlocks [

	self == anotherValue ifTrue: [ ^ self ].
	^ anotherValue joinNonNull: self onIRGenerator: anIRGenerator from: originBlocks
]

{ #category : #accessing }
DRValue >> label [
	
	^ self name
]

{ #category : #interpretation }
DRValue >> lookupClass [
	
	^ Integer
]

{ #category : #accessing }
DRValue >> operands [
	
	^ #()
]

{ #category : #'users-definitions' }
DRValue >> removeUser: anInstruction [
	
	"Nothing"
]

{ #category : #'users-definitions' }
DRValue >> replaceUsersBy: anotherInstruction [
	
	"Nothing"
]

{ #category : #'users-definitions' }
DRValue >> replaceUsesBy: anotherInstruction [
	
	"Nothing"
]

{ #category : #accessing }
DRValue >> result [

	^ self
]

{ #category : #'cogit-generation' }
DRValue >> rtlOperandQualifier [

	self subclassResponsibility
]

{ #category : #'cogit-generation' }
DRValue >> rtlOperandQualifierForInstruction: instruction [

	^ self rtlOperandQualifier
]

{ #category : #transforming }
DRValue >> simplify [
	
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

{ #category : #dependencies }
DRValue >> withAllDependencies [
	
	^ { self }
]
