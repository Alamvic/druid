Class {
	#name : 'DRUnaryInstruction',
	#superclass : 'DRInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'SCCP' }
DRUnaryInstruction >> sccpLatticeValueFor: sccp [

	| operand1 |
	
	operand1 := sccp latticeValue: self operand1. 
	
	(({ sccp top. sccp bottom } includes: operand1) not 
		and: [ operand1 isConstant ])
				ifTrue: [ 
					"Compute the lattice value and wrap it as a DR value"
					^ (self sccpOperateOnLattice: operand1 value) asDRValue ].

	operand1 = sccp bottom ifTrue: [ ^ sccp bottom ].
	^ sccp top
]

{ #category : 'SCCP' }
DRUnaryInstruction >> sccpOperateOnLattice: operand [
	
	self subclassResponsibility
]
