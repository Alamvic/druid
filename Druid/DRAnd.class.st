Class {
	#name : #DRAnd,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRAnd >> acceptVisitor: aVisitor [

	^ aVisitor visitAnd: self
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
	operand2 value = true ifTrue: [ ^ operand1  ].
	
	^ super sccpLatticeValueFor: sccp
]

{ #category : #SCCP }
DRAnd >> sccpOperateOnLattice: aBoolean and: anotherBoolean [

	^ aBoolean and: anotherBoolean
]
