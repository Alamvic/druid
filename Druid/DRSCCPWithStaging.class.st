Class {
	#name : #DRSCCPWithStaging,
	#superclass : #DRSCCP,
	#category : #'Druid-Optimizations'
}

{ #category : #'special cases' }
DRSCCPWithStaging >> latticeForLoad: aLoad [

	| addressLattice |
	aLoad isJITCompileTimeExpression ifTrue: [
		| newVar |
		newVar := aLoad result name asLowercase.
		aLoad replaceUsesBy: newVar asDRValue asJitCompileTimeExpression.
		^ DRStore
			  operands: {
					  aLoad size asDRValue.
					  aLoad address asJitCompileTimeExpression.
					  (DRInterpreterExpression reference: newVar) }
			  result: DRNoRegister new ].

	addressLattice := aLoad address sccpLatticeValueFor: self.

	(self isConstantLattice: addressLattice) ifTrue: [ "If the address is constant, we can move the load to JIT compile time"
		^ 'coInterpreter int64AtPointer:' asDRValue asJitCompileTimeExpression
		  , addressLattice asDRValue asJitCompileTimeExpression ].

	"sccp value is the same that the address, either top or bottom"
	^ addressLattice
]

{ #category : #lattice }
DRSCCPWithStaging >> latticeValue: aValue [

	| lattice |
	lattice := super latticeValue: aValue.
	(lattice = self top and: [ aValue isInstruction not ])
		ifTrue: [ ^ aValue ].
	^ lattice
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
