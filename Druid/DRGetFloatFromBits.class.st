Class {
	#name : #DRGetFloatFromBits,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRGetFloatFromBits >> acceptVisitor: aVisitor [
	
	^ aVisitor visitGetFloatFromBits: self
]

{ #category : #printing }
DRGetFloatFromBits >> opcode [
	
	^ 'GetFloatFromBits'
]

{ #category : #SCCP }
DRGetFloatFromBits >> sccpLatticeValueFor: sccp [

	| lattice |
	lattice := sccp latticeValue: operands first.
	(sccp isConstantLattice: lattice) ifFalse: [ ^ lattice ].
	^ Float fromIEEE64BitWord: lattice
]

{ #category : #'constant folding' }
DRGetFloatFromBits >> simpleConstantFold [
	
	^ Float fromIEEE64BitWord: self operand1 simpleConstantFold
]

{ #category : #types }
DRGetFloatFromBits >> type [
	
	^ DRFloatType new
]
