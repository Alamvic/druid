Class {
	#name : #DRRegisterCollapse,
	#superclass : #DROptimisation,
	#category : #'Druid-BytecodeGeneration'
}

{ #category : #accessing }
DRRegisterCollapse >> applyTo: cfg [

	cfg reversePostOrderBlocks do: [ :block |
		block instructionsDo: [ :i |
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
				ifFalse: [
					i isPhiFunction ifTrue: [
						i users do: [ :user |
							user isPhiFunction ifTrue: [
								
								self assert: i basicBlock ~= user basicBlock.
								
								i basicBlock addInstruction:
									(DRStoreTemporaryVariable operands: {
											 i result name asLowercase asDRValue.
											 i }).

								user replaceOperand: i by: (DRLoadTemporaryVariable operands:
										 { i result name asLowercase asDRValue }) ] ] ].
					i basicBlock removeInstruction: i ] ] ]
]
