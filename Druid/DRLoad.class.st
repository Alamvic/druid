Class {
	#name : #DRLoad,
	#superclass : #DRInstruction,
	#instVars : [
		'size',
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

{ #category : #types }
DRLoad >> beFloatingPoint64 [
	
	type := DRFloatType new
]

{ #category : #testing }
DRLoad >> canMoveOutsideLoop: backJump [

	^ false
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
	
	^ 'LOAD ', size asString
]

{ #category : #accessing }
DRLoad >> operands: operandList [

	size := operandList first simpleConstantFold.
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

	| address |
	address := self operand1 sccpLatticeValueFor: sccp.
	
	(sccp isConstantLattice: address) ifTrue: [ 
		"If the address is constant, we can move the load to JIT compile time"
		^ 'coInterpreter int64AtPointer:' asDRValue asJitCompileTimeExpression , address asDRValue asJitCompileTimeExpression ].

	"sccp value is the same that the address"
	^ address
]

{ #category : #accessing }
DRLoad >> size [

	^ size
]

{ #category : #accessing }
DRLoad >> type [
	
	^ type
]
