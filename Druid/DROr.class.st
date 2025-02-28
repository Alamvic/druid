Class {
	#name : 'DROr',
	#superclass : 'DRBooleanInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DROr >> acceptVisitor: aVisitor [

	^ aVisitor visitOr: self
]

{ #category : 'SCCP' }
DROr >> asNumberedExpression [

	| numberedExpression |
	
	numberedExpression := DRNumberedExpression new.
	^ numberedExpression operation: self class;
	operands: self operands asBag;
	yourself
]

{ #category : 'printing' }
DROr >> opcode [
	
	^ 'OR'
]

{ #category : 'SCCP' }
DROr >> sccpLatticeValueFor: sccp [

	| operand1 operand2 |
	operand1 := sccp latticeValue: self operand1. 
	operand2 := sccp latticeValue: self operand2.
	
	(operand1 value = true
		or: [ operand2 value = true ])
			ifTrue: [ ^ true asDRValue ].

	operand1 value = false ifTrue: [ ^ self sccpLatticeValueOperand: 2 for: sccp ].	
	operand2 value = false ifTrue: [ ^ self sccpLatticeValueOperand: 1 for: sccp ].

	^ super sccpLatticeValueFor: sccp
]

{ #category : 'SCCP' }
DROr >> sccpOperateOnLattice: aBoolean and: anotherBoolean [

	^ aBoolean or: anotherBoolean
]
