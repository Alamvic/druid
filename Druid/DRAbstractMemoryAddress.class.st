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

{ #category : #'users-definitions' }
DRAbstractMemoryAddress >> replaceDependency: anOperand by: anotherOperand [

	self dependencies do: [ :e | 
		e replaceDependency: anOperand by: anotherOperand ]
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
