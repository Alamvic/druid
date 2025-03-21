Class {
	#name : 'DRFloatToInt',
	#superclass : 'DRInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRFloatToInt >> acceptVisitor: aVisitor [
	
	^ aVisitor visitFloatToIntConversion: self
]

{ #category : 'printing' }
DRFloatToInt >> opcode [
	
	^ 'FloatToInt'
]

{ #category : 'SCCP' }
DRFloatToInt >> sccpLatticeValueFor: sccp [

	| lattice |
	lattice := (sccp latticeValue: operands first).
	(sccp isConstantLattice: lattice)
		ifFalse: [ ^ lattice ].
	^ lattice value asInteger asDRValue
]

{ #category : 'constant folding' }
DRFloatToInt >> simpleConstantFold [
	
	^ self operand1 simpleConstantFold asInteger
]
