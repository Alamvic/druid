Class {
	#name : #DRPush,
	#superclass : #DRInstruction,
	#category : #'Druid-BytecodeGeneration'
}

{ #category : #visiting }
DRPush >> acceptVisitor: aVisitor [

	^ aVisitor visitPush: self
]

{ #category : #testing }
DRPush >> isMandatoryInstruction [

	^ true
]

{ #category : #testing }
DRPush >> isPush [
	
	^ true
]

{ #category : #printing }
DRPush >> opcode [
	
	^ 'Push'
]

{ #category : #'users-definitions' }
DRPush >> replaceDependency: anOperand by: anotherOperand [

	"A push instruction can have two kind of operands.
	It is either a value, in which case we should replace the operand.
	Or it is a composite value (e.g., an address) and we should replace the operand inside the address"
	self operand1
		replaceDependency: anOperand
		by: anotherOperand
		in: self
]

{ #category : #SCCP }
DRPush >> sccpLatticeValueFor: sccp [ 
	
	"This instruction has no value"
	^ sccp bottom
]

{ #category : #SCCP }
DRPush >> size [
	"Pushed memory size"
	
	^ 8 "bytes"
]
