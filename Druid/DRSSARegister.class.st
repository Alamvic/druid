Class {
	#name : 'DRSSARegister',
	#superclass : 'DRRegister',
	#instVars : [
		'physicalRegister'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'converting' }
DRSSARegister >> asJitCompileTimeExpression [
	
	^ self prettyPrint
]

{ #category : 'testing' }
DRSSARegister >> hasId: anId [

	^ name = anId
]

{ #category : 'accessing' }
DRSSARegister >> id [
	^ name
]

{ #category : 'testing' }
DRSSARegister >> isRegister [

	^ true
]

{ #category : 'testing' }
DRSSARegister >> isSSARegister [
	
	^ true
]

{ #category : 'testing' }
DRSSARegister >> isValue [

	^ false
]

{ #category : 'accessing' }
DRSSARegister >> name [
	^ 'ssa_', name asString
]

{ #category : 'accessing' }
DRSSARegister >> physicalRegister: anObject [

	physicalRegister := anObject
]

{ #category : 'accessing' }
DRSSARegister >> prettyPrint [

	^ 't', name asString
]

{ #category : 'accessing' }
DRSSARegister >> printOn: aStream [

	aStream
		nextPutAll: self class name;
		nextPutAll: '(';
		nextPutAll: self name asString;
		nextPutAll: ')'
]

{ #category : 'accessing' }
DRSSARegister >> result [

	physicalRegister ifNotNil: [ ^ physicalRegister ].
	^ self
]

{ #category : 'cogit-generation' }
DRSSARegister >> rtlOperandQualifier [
	
	"If we are using a temporary register,
	it's a compile time register that's known at compile time as a constant"
	^ 'Cq:'
]

{ #category : 'type' }
DRSSARegister >> type [
	"Maybe this should be parametrised"

	^ DRSignedIntegerType size: 8 "Word size"
]
