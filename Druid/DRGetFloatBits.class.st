Class {
	#name : 'DRGetFloatBits',
	#superclass : 'DRInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRGetFloatBits >> acceptVisitor: aVisitor [
	
	^ aVisitor visitGetFloatBits: self
]

{ #category : 'printing' }
DRGetFloatBits >> opcode [
	
	^ 'GetFloatBits'
]

{ #category : 'SCCP' }
DRGetFloatBits >> sccpLatticeValueFor: sccp [

	| lattice |
	lattice := sccp latticeValue: operands first.
	(sccp isConstantLattice: lattice) ifFalse: [ ^ lattice ].
	^ lattice asIEEE64BitWord
]

{ #category : 'constant folding' }
DRGetFloatBits >> simpleConstantFold [

	| folded |
	folded := self operand1 simpleConstantFold.
	folded isNumber ifTrue: [ ^ folded asIEEE64BitWord ].
	^ folded
]
