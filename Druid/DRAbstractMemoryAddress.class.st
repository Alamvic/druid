Class {
	#name : #DRAbstractMemoryAddress,
	#superclass : #DRValue,
	#category : #'Druid-IR'
}

{ #category : #'meta-data' }
DRAbstractMemoryAddress >> addUser: anInstruction [
	
	self dependencies do: [ :e | e addUser: anInstruction ]
]

{ #category : #testing }
DRAbstractMemoryAddress >> isAbsolute [
	
	^ false
]

{ #category : #testing }
DRAbstractMemoryAddress >> isMemoryAddress [
	
	^ true
]

{ #category : #accessing }
DRAbstractMemoryAddress >> operands [

	self subclassResponsibility
]

{ #category : #'meta-data' }
DRAbstractMemoryAddress >> removeUser: anInstruction [
	
	self dependencies do: [ :e | e removeUser: anInstruction ]
]

{ #category : #'users-definitions' }
DRAbstractMemoryAddress >> replaceDependency: anOperand by: anotherOperand [

	self dependencies do: [ :e | 
		e replaceDependency: anOperand by: anotherOperand ]
]

{ #category : #dependencies }
DRAbstractMemoryAddress >> replaceDependency: anOperand by: anotherOperand in: anInstruction [
	
	anOperand removeUser: anInstruction.
	anotherOperand addUser: anInstruction.
	self replaceDependency: anOperand by: anotherOperand.
]

{ #category : #cogit }
DRAbstractMemoryAddress >> rtlArgumentExpressions [
	
	self subclassResponsibility
]

{ #category : #'cogit-generation' }
DRAbstractMemoryAddress >> rtlOperandQualifierForInstruction: instruction [

	| bytesSuffix |
	bytesSuffix := instruction size = 1
		               ifTrue: [ 'b' ]
		               ifFalse: [ (instruction size * 8) asString "bytes" ].

	^ 'M' , bytesSuffix , ':r:'
]

{ #category : #cogit }
DRAbstractMemoryAddress >> rtlPushArgumentExpressions [
	
	"Normal load and store instructions have offset, then base.
	Push instructions have base, then offset"
	^ self rtlArgumentExpressions reversed
]

{ #category : #cogit }
DRAbstractMemoryAddress >> rtlPushOperandQualifierForInstruction: aPush [

	^ 'Base:offset:'
]
