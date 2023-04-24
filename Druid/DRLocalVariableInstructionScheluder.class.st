Class {
	#name : #DRLocalVariableInstructionScheluder,
	#superclass : #DROptimisation,
	#category : #'Druid-BytecodeGeneration'
}

{ #category : #operations }
DRLocalVariableInstructionScheluder >> applyTo: cfg [

	cfg instructionsDo: [ :i |
		i users size > 1 ifTrue: [ self storeReusedResultOf: i ] ]
]

{ #category : #transformation }
DRLocalVariableInstructionScheluder >> storeReusedResultOf: aDRInstruction [

	self assert: aDRInstruction result isRegister.

	aDRInstruction replaceUsesBy: (DRLoadTemporaryVariable operands:
			 { aDRInstruction result name asLowercase asDRValue }).
	aDRInstruction basicBlock
		add: (DRStoreTemporaryVariable operands: {
					 aDRInstruction result name asLowercase asDRValue.
					 aDRInstruction })
		after: aDRInstruction
]
