Class {
	#name : 'DRPush',
	#superclass : 'DRStackEffect',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRPush >> acceptVisitor: aVisitor [

	^ aVisitor visitPush: self
]

{ #category : 'testing' }
DRPush >> isPush [
	
	^ true
]

{ #category : 'printing' }
DRPush >> opcode [
	
	^ 'Push'
]

{ #category : 'copying' }
DRPush >> postCopy [

	super postCopy.
	self operand1 isMemoryAddress ifTrue: [
		self operand1: self operand1 copy ]
]

{ #category : 'users-definitions' }
DRPush >> replaceDependency: anOperand by: anotherOperand [

	"A push instruction can have two kind of operands.
	It is either a value, in which case we should replace the operand.
	Or it is a composite value (e.g., an address) and we should replace the operand inside the address"
	self operand1
		replaceDependency: anOperand
		by: anotherOperand
		in: self
]

{ #category : 'SCCP' }
DRPush >> sccpLatticeValueFor: sccp [ 
	
	"This instruction has no value"
	^ sccp bottom
]

{ #category : 'SCCP' }
DRPush >> size [
	"Pushed memory size"
	
	^ 8 "bytes"
]

{ #category : 'accessing' }
DRPush >> stackDelta [
	
	"I add one element to the stack"
	^ -1
]

{ #category : 'testing' }
DRPush >> stackDepth [
	
	"Return the stack depth left on the stack after applying this operation.
	Push will add one element to the stack depth of the previous stack instruction"
	
	stackDependencies ifEmpty: [ ^ 1 ].
	^ stackDependencies anyOne stackDepth + 1
]

{ #category : 'as yet unclassified' }
DRPush >> stackSlotsDefiningAtDepth: anInteger [

	anInteger = 0 ifTrue: [ ^ { self } ].
	^ super stackSlotsDefiningAtDepth: anInteger
]
