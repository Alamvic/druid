Class {
	#name : #DRTemporaryRegister,
	#superclass : #DRValue,
	#instVars : [
		'id',
		'physicalRegister'
	],
	#category : #'Druid-IR'
}

{ #category : #'instance-creation' }
DRTemporaryRegister class >> id: anInteger [ 
	
	^ self new
		id: anInteger;
		yourself
]

{ #category : #accessing }
DRTemporaryRegister >> = aRegister [

	(self isKindOf: aRegister species)
		ifFalse: [ ^ false ].
	^ self id = aRegister id
]

{ #category : #converting }
DRTemporaryRegister >> asJitCompileTimeExpression [
	
	^ ('t', id asString) asDRValue asJitCompileTimeExpression
]

{ #category : #testing }
DRTemporaryRegister >> hasId: anId [

	^ id = anId
]

{ #category : #accessing }
DRTemporaryRegister >> hash [

	^ id hash
]

{ #category : #accessing }
DRTemporaryRegister >> id [
	^ id
]

{ #category : #accessing }
DRTemporaryRegister >> id: anInteger [ 
	id := anInteger
]

{ #category : #testing }
DRTemporaryRegister >> isRegister [

	^ true
]

{ #category : #testing }
DRTemporaryRegister >> isValue [

	^ false
]

{ #category : #accessing }
DRTemporaryRegister >> isVirtualRegister [
	
	^ true
]

{ #category : #accessing }
DRTemporaryRegister >> name [

	^ 'R' , self id asString
]

{ #category : #accessing }
DRTemporaryRegister >> physicalRegister [

	^ physicalRegister
]

{ #category : #accessing }
DRTemporaryRegister >> physicalRegister: anObject [

	physicalRegister := anObject
]

{ #category : #accessing }
DRTemporaryRegister >> printOn: aStream [

	aStream
		nextPutAll: self class name;
		nextPutAll: '(t';
		nextPutAll: self id asString;
		nextPutAll: ')'
]

{ #category : #accessing }
DRTemporaryRegister >> result [

	physicalRegister ifNotNil: [ ^ physicalRegister ].
	^ self
]

{ #category : #'cogit-generation' }
DRTemporaryRegister >> rtlOperandQualifier [
	
	^ 'R:'
]

{ #category : #type }
DRTemporaryRegister >> type [
	"Maybe this should be parametrised"

	^ DRSignedIntegerType size: 8 "Word size"
]
