Class {
	#name : #DRConstantValue,
	#superclass : #DRValue,
	#instVars : [
		'value'
	],
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRConstantValue class >> value: anInteger [
	
	^ self new
		value: anInteger;
		yourself
]

{ #category : #testing }
DRConstantValue >> = anObject [

	self class = anObject class ifTrue: [ 
		self value = anObject value ifTrue: [ ^true. ] ].
	^ false. 
]

{ #category : #'rtl-translation' }
DRConstantValue >> cogitTrueConditionCode [
	
	"Jump false if the constant is a zero"
	^ #JumpZero:
]

{ #category : #copying }
DRConstantValue >> copyToVariableIn: aDRIRGenerator inNode: aNode [
	
	^ aDRIRGenerator
		addInstructionFrom: aNode
		instructionKind: DRCopy
		operands: { self }.
]

{ #category : #testing }
DRConstantValue >> isConstant [
	
	^ true
]

{ #category : #interpretation }
DRConstantValue >> lookupClass [
	
	^ value class
]

{ #category : #accessing }
DRConstantValue >> name [

	^ value asString
]

{ #category : #'cogit-generation' }
DRConstantValue >> rtlOperandQualifier [
	
	^ 'Cq:'
]

{ #category : #accessing }
DRConstantValue >> value [
	^ value
]

{ #category : #accessing }
DRConstantValue >> value: anInteger [ 
	value := anInteger
]
