Class {
	#name : #DRLocalVariableInstructionScheluder,
	#superclass : #DROptimisation,
	#category : #'Druid-BytecodeGeneration'
}

{ #category : #operations }
DRLocalVariableInstructionScheluder >> applyTo: cfg [

	cfg instructionsDo: [ :i |
		(self shouldIgnore: i) ifFalse: [
			i users size > 1 ifTrue: [ self storeReusedResultOf: i ] ] ]
]

{ #category : #operations }
DRLocalVariableInstructionScheluder >> shouldIgnore: aDRInstruction [

	^ aDRInstruction isStore or: [
		  aDRInstruction isPhiFunction and: [
			  aDRInstruction operands allSatisfy: [ :op | op isStore ] ] ]
]

{ #category : #transformation }
DRLocalVariableInstructionScheluder >> storeReusedResultOf: aDRInstruction [

	self assert: aDRInstruction result isRegister.

	aDRInstruction replaceUsesBy: (DRLoadTemporaryVariable operands: {
				 aDRInstruction result name asString asLowercase asDRValue.
				 "store" }).

	aDRInstruction basicBlock
		add: (DRStoreTemporaryVariable
				 operands: {
						 aDRInstruction result name asString asLowercase asDRValue.
						 aDRInstruction }
				 result: DRNoRegister new)
		after: aDRInstruction
]
