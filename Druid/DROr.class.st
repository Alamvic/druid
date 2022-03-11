Class {
	#name : #DROr,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DROr >> acceptVisitor: aVisitor [

	^ aVisitor visitOr: self
]

{ #category : #printing }
DROr >> opcode [
	
	^ 'OR'
]

{ #category : #SCCP }
DROr >> sccpLatticeValueFor: sccp [

	| operand1 operand2 |
	operand1 := sccp latticeValue: self operand1. 
	operand2 := sccp latticeValue: self operand2.
	
	(operand1 value = true
		or: [ operand2 value = true ])
			ifTrue: [ ^ true asDRValue ].
	operand2 value = false ifTrue: [ 1halt. ^ operand1  ].
	
	^ super sccpLatticeValueFor: sccp
]

{ #category : #SCCP }
DROr >> sccpOperateOnLattice: aBoolean and: anotherBoolean [

	^ aBoolean or: anotherBoolean
]
