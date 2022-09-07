Class {
	#name : #DRLoad,
	#superclass : #DRInstruction,
	#instVars : [
		'size'
	],
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLoad >> acceptVisitor: aVisitor [
	
	^ aVisitor visitLoad: self
]

{ #category : #'meta-data' }
DRLoad >> address [

	^ self operand1
]

{ #category : #'meta-data' }
DRLoad >> address: anAddress [

	^ self operand1: anAddress
]

{ #category : #types }
DRLoad >> beFloatingPoint64 [
	
	type := DRFloatType new
]

{ #category : #'meta-data' }
DRLoad >> dependencies [

	^ self address dependencies
]

{ #category : #'users-definitions' }
DRLoad >> hasDependency: anInstruction [
	
	^ self dependencies includes: anInstruction
]

{ #category : #accessing }
DRLoad >> initialize [

	super initialize.
	type := DRIntegerType new
]

{ #category : #visiting }
DRLoad >> isLoad [

	^ true
]

{ #category : #printing }
DRLoad >> opcode [
	
	^ 'LOAD'
]

{ #category : #accessing }
DRLoad >> operands: operandList [

	size := operandList first value.
	super operands: { operandList second asMemoryAddress }
]

{ #category : #'users-definitions' }
DRLoad >> replaceDependency: anOperand by: anotherOperand [

	self address replaceDependency: anOperand by: anotherOperand
]

{ #category : #accessing }
DRLoad >> size [

	^ size
]

{ #category : #accessing }
DRLoad >> type [
	
	^ type
]
