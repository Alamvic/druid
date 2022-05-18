Class {
	#name : #DRAnd,
	#superclass : #DRBooleanInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRAnd >> acceptVisitor: aVisitor [

	^ aVisitor visitAnd: self
]

{ #category : #SCCP }
DRAnd >> asNumberedExpression [

	| numberedExpression |
	
	numberedExpression := DRNumberedExpression new.
	^ numberedExpression operation: self class;
	operands: self operands asBag;
	yourself
]

{ #category : #visiting }
DRAnd >> cogitFalseConditionCode [
	
	"Jump false if the constant is a zero"
	^ #JumpZero:
]

{ #category : #visiting }
DRAnd >> cogitTrueConditionCode [
	
	"Jump false if the constant is a zero"
	^ #JumpNonZero:
]

{ #category : #printing }
DRAnd >> opcode [
	
	^ 'AND'
]

{ #category : #SCCP }
DRAnd >> sccpLatticeValueFor: sccp [

	| operand1 operand2 |
	operand1 := sccp latticeValue: self operand1. 
	operand2 := sccp latticeValue: self operand2.
	
	(operand1 value = false
		or: [ operand2 value = false ])
			ifTrue: [ ^ false asDRValue ].

	operand1 value = true ifTrue: [ ^ self sccpLatticeValueOperand: 2 for: sccp ].	
	operand2 value = true ifTrue: [ ^ self sccpLatticeValueOperand: 1 for: sccp ].
	
	^ super sccpLatticeValueFor: sccp
]

{ #category : #SCCP }
DRAnd >> sccpOperateOnLattice: aBoolean and: anotherBoolean [

	^ aBoolean and: anotherBoolean
]
