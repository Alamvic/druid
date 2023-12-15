Class {
	#name : #DRSCCPWithStaging,
	#superclass : #DRSCCP,
	#category : #'Druid-Optimizations'
}

{ #category : #lattice }
DRSCCPWithStaging >> latticeValue: aValue [

	aValue isJITCompileTimeExpression ifTrue: [ ^ aValue ].
	^ super latticeValue: aValue
]

{ #category : #lattice }
DRSCCPWithStaging >> sccpLatticeValueFor: instruction latticeOperand1: operand1 latticeOperand2: operand2 [

	((({ self top. self bottom } includes: operand1) not
		and: [ ({ self top. self bottom } includes: operand2) not ])
		and: [ operand1 isRegister not and: [ operand2 isRegister not ] ])
				ifTrue: [ |op1 op2|
					op1 := operand1.
					op2 := operand2 .
					"If they have different values, choose one to continue the work"
					operand1 class ~= operand2 class
						ifTrue: [ 
							op2 := operand1 class tryParseOperandForSCCP: op2.
							(self isConstantLattice: op2) ifFalse: [ ^ op2 ].
							op1 := operand2 class tryParseOperandForSCCP: op1.
							(self isConstantLattice: op1) ifFalse: [ ^ op1 ]
						].
					"Compute the lattice value and wrap it as a DR value"
					^ (instruction sccpOperateOnLattice: op1 value and: op2 value) asDRValue ].

	^ super sccpLatticeValueFor: instruction latticeOperand1: operand1 latticeOperand2: operand2
]
