Class {
	#name : #DRLoad,
	#superclass : #DRAbstractLoad,
	#instVars : [
		'type'
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

{ #category : #converting }
DRLoad >> asJitCompileTimeExpression [

	self assert: self isJITCompileTimeExpression.
	^ self address asJitCompileTimeExpression
]

{ #category : #types }
DRLoad >> beFloatingPoint64 [
	1halt.
	type := DRFloatType new
]

{ #category : #testing }
DRLoad >> canMoveOutsideLoop: backJump [

	^ false
]

{ #category : #testing }
DRLoad >> isJITCompileTimeExpression [

	^ self address isStaged
]

{ #category : #visiting }
DRLoad >> isLoad [

	^ true
]

{ #category : #printing }
DRLoad >> opcode [
	
	^ 'LOAD ', type asString
]

{ #category : #accessing }
DRLoad >> operands: operandList [

	type := operandList first.
	self haltIf: [ (type isKindOf: DRIntegerType) not ].
	super operands: { operandList second asMemoryAddress }
]

{ #category : #'users-definitions' }
DRLoad >> replaceDependency: anOperand by: anotherOperand [

	anOperand removeUser: self.
	anotherOperand addUser: self.
	self address replaceDependency: anOperand by: anotherOperand
]

{ #category : #SCCP }
DRLoad >> sccpLatticeValueFor: sccp [

	^ sccp latticeForLoad: self
]

{ #category : #accessing }
DRLoad >> size [

	^ type size
]

{ #category : #accessing }
DRLoad >> type [
	
	^ type
]
