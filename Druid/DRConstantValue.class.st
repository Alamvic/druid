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

{ #category : #visiting }
DRConstantValue >> acceptVisitor: aVisitor [

	^ aVisitor visitConstant: self
]

{ #category : #'rtl-translation' }
DRConstantValue >> cogitFalseConditionCode [
	
	"Jump false if the constant is a zero"
	^ #JumpNonZero:
]

{ #category : #'rtl-translation' }
DRConstantValue >> cogitTrueConditionCode [
	
	"Jump false if the constant is a zero"
	^ #JumpZero:
]

{ #category : #testing }
DRConstantValue >> hash [

	^ value hash
]

{ #category : #accessing }
DRConstantValue >> includes: anInstruction [ 
	
	^ false
]

{ #category : #testing }
DRConstantValue >> isConstant [
	
	^ true
]

{ #category : #accessing }
DRConstantValue >> isCopy [ 

	^ false
]

{ #category : #interpretation }
DRConstantValue >> lookupClass [
	
	^ value class
]

{ #category : #accessing }
DRConstantValue >> name [

	^ value asString
]

{ #category : #printing }
DRConstantValue >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: '(';
		print: value;
		nextPutAll: ')'
]

{ #category : #'cogit-generation' }
DRConstantValue >> rtlOperandQualifier [
	
	^ 'Cq:'
]

{ #category : #accessing }
DRConstantValue >> sccpLatticeValueFor: sccp [

	^ self
]

{ #category : #'constant folding' }
DRConstantValue >> simpleConstantFold [
	
	^ value
]

{ #category : #types }
DRConstantValue >> type [
	
	value = nil ifTrue: [ ^ DRTopType new ].
	(#(true false) includes: value) ifTrue: [ ^ DRBooleanType new ].
	^ 1halt
]

{ #category : #accessing }
DRConstantValue >> value [
	^ value
]

{ #category : #accessing }
DRConstantValue >> value: anInteger [ 

	value := anInteger
]
