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

	^ self address isJITCompileTimeExpression
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
	self haltIf: [ (type isKindOf: DRTopType) not ].
	super operands: { operandList second asMemoryAddress }
]

{ #category : #'users-definitions' }
DRLoad >> replaceDependency: anOperand by: anotherOperand [

	anOperand removeUser: self.
	anotherOperand addUser: self.
	self address replaceDependency: anOperand by: anotherOperand
]

{ #category : #'cogit-generation' }
DRLoad >> rtlOperandQualifier [
	
	^ result rtlOperandQualifier
]

{ #category : #SCCP }
DRLoad >> sccpLatticeValueFor: sccp [

	| address |
	self isJITCompileTimeExpression ifTrue: [
		| newVar |
		newVar := self result name asLowercase.
		self replaceUsesBy: newVar asDRValue asJitCompileTimeExpression.
		^ DRStore
			  operands: {
					  self size asDRValue.
					  self address asJitCompileTimeExpression.
					  (DRInterpreterExpression reference: newVar) }
			  result: DRNoRegister new ].

	address := self address sccpLatticeValueFor: sccp.

	(sccp isConstantLattice: address) ifTrue: [ "If the address is constant, we can move the load to JIT compile time"
		^ 'coInterpreter int64AtPointer:' asDRValue asJitCompileTimeExpression
		  , address asDRValue asJitCompileTimeExpression ].

	"sccp value is the same that the address"
	^ address
]

{ #category : #accessing }
DRLoad >> size [

	^ type size
]

{ #category : #accessing }
DRLoad >> type [
	
	^ type
]
