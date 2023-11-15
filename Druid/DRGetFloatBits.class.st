Class {
	#name : #DRGetFloatBits,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRGetFloatBits >> acceptVisitor: aVisitor [
	
	^ aVisitor visitGetFloatBits: self
]

{ #category : #printing }
DRGetFloatBits >> opcode [
	
	^ 'GetFloatBits'
]

{ #category : #SCCP }
DRGetFloatBits >> sccpLatticeValueFor: sccp [

	| lattice |
	lattice := sccp latticeValue: operands first.
	(sccp isConstantLattice: lattice) ifFalse: [ ^ lattice ].
	^ lattice asIEEE64BitWord
]

{ #category : #'constant folding' }
DRGetFloatBits >> simpleConstantFold [
	
	^ self operand1 simpleConstantFold asIEEE64BitWord
]

{ #category : #types }
DRGetFloatBits >> type [
	
	^ DRSignedIntegerType new
]
