Class {
	#name : #DRRegisterCollapse,
	#superclass : #DROptimisation,
	#category : #'Druid-BytecodeGeneration'
}

{ #category : #accessing }
DRRegisterCollapse >> applyTo: cfg [

	cfg instructionsDo: [ :i |
		i isMandatoryInstruction
			ifTrue: [
				i result isRegister ifTrue: [
					i replaceUsesBy: (DRLoadTemporaryVariable operands:
							 { i result name asLowercase asDRValue }).
					i basicBlock
						add: (DRStoreTemporaryVariable operands: {
									 i result name asLowercase asDRValue.
									 i })
						after: i.
					i basicBlock removeInstruction: i ] ]
			ifFalse: [ i basicBlock removeInstruction: i ] ]
]
