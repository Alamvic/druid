Class {
	#name : 'DRIntToFloat',
	#superclass : 'DRInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRIntToFloat >> acceptVisitor: aVisitor [
	
	^ aVisitor visitIntToFloatConversion: self
]

{ #category : 'printing' }
DRIntToFloat >> opcode [
	
	^ 'IntToFloat'
]

{ #category : 'SCCP' }
DRIntToFloat >> sccpLatticeValueFor: sccp [

	| lattice |
	lattice := (sccp latticeValue: operands first).
	(sccp isConstantLattice: lattice)
		ifFalse: [ ^ lattice ].
	^ lattice asFloat
]

{ #category : 'constant folding' }
DRIntToFloat >> simpleConstantFold [
	
	^ self operand1 simpleConstantFold asFloat
]

{ #category : 'types' }
DRIntToFloat >> typeWithAlreadySeen: _ [
	
	^ DRFloatType new
]
