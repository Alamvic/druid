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

	^ aDRInstruction isPhiFunction and: [
		  aDRInstruction operands allSatisfy: [ :op | op isStore ] ]
]

{ #category : #transformation }
DRLocalVariableInstructionScheluder >> storeReusedResultOf: aDRInstruction [

	| store |
	self assert: aDRInstruction result isRegister.

	store := DRStoreTemporaryVariable
		         operands: {
				         aDRInstruction result name asLowercase asDRValue.
				         aDRInstruction }
		         result: DRNoRegister new.

	aDRInstruction replaceUsesBy: (DRLoadTemporaryVariable operands: {
				 aDRInstruction result name asLowercase asDRValue.
				 store }).
				
	aDRInstruction basicBlock add: store after: aDRInstruction
]
