Class {
	#name : 'DRConstantValue',
	#superclass : 'DRValue',
	#instVars : [
		'value'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'accessing' }
DRConstantValue class >> value: anInteger [
	
	^ self new
		value: anInteger;
		yourself
]

{ #category : 'testing' }
DRConstantValue >> = anObject [

	self class = anObject class ifTrue: [ 
		self value = anObject value ifTrue: [ ^true. ] ].
	^ false. 
]

{ #category : 'visiting' }
DRConstantValue >> acceptVisitor: aVisitor [

	^ aVisitor visitConstant: self
]

{ #category : 'converting' }
DRConstantValue >> asJitCompileTimeExpression [

	^ DRInterpreterReference reference: '(' , self value asString , ')'
]

{ #category : 'accessing' }
DRConstantValue >> expression [
	
	^ value asString
]

{ #category : 'testing' }
DRConstantValue >> hash [

	^ value hash
]

{ #category : 'accessing' }
DRConstantValue >> includes: anInstruction [ 
	
	^ false
]

{ #category : 'testing' }
DRConstantValue >> isConstant [
	
	^ true
]

{ #category : 'accessing' }
DRConstantValue >> isCopy [ 

	^ false
]

{ #category : 'loop-invariance' }
DRConstantValue >> isInvariantFrom: loop [
	^ true
]

{ #category : 'testing' }
DRConstantValue >> isJITCompileTimeExpression [
	
	^ value isStaged
]

{ #category : 'testing' }
DRConstantValue >> isJITCompileTimeExpressionIgnoring: aCollection [ 

	^ self isJITCompileTimeExpression
]

{ #category : 'testing' }
DRConstantValue >> isNumberValue [

	^ value isNumber
]

{ #category : 'testing' }
DRConstantValue >> jitCompileTimeExpression [

	^ RBLiteralValueNode value: value
]

{ #category : 'interpretation' }
DRConstantValue >> lookupClass [
	
	^ value class
]

{ #category : 'accessing' }
DRConstantValue >> name [

	^ value asString
]

{ #category : 'printing' }
DRConstantValue >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: '(';
		print: value;
		nextPutAll: ')'
]

{ #category : 'cogit-generation' }
DRConstantValue >> rtlOperandQualifier [
	
	^ 'Cq:'
]

{ #category : 'accessing' }
DRConstantValue >> sccpLatticeValueFor: sccp [

	^ self
]

{ #category : 'constant folding' }
DRConstantValue >> simpleConstantFold [
	
	^ value
]

{ #category : 'types' }
DRConstantValue >> type [

	value = nil ifTrue: [ ^ DRUndefinedType new ].
	(#( true false ) includes: value) ifTrue: [ ^ DRBooleanType new ].
	value isInteger ifTrue: [ ^ DRSignedIntegerType size: 8 "Word size" ].
	value isFloat ifTrue: [ ^ DRFloatType new ].
	^ value class
]

{ #category : 'accessing' }
DRConstantValue >> value [
	^ value
]

{ #category : 'accessing' }
DRConstantValue >> value: anInteger [ 

	value := anInteger
]
