Class {
	#name : 'DRLocalVariableInstructionScheluder',
	#superclass : 'DROptimisation',
	#category : 'Druid-BytecodeGeneration',
	#package : 'Druid',
	#tag : 'BytecodeGeneration'
}

{ #category : 'operations' }
DRLocalVariableInstructionScheluder >> applyTo: cfg [

	self replacePhisByStores: cfg.
	self storeResultsToBeReused: cfg.

]

{ #category : 'transformation' }
DRLocalVariableInstructionScheluder >> replacePhisByStores: aDRControlFlowGraph [
	"when we replace phi by store/temp.
	We should add the created temps to the scope"
	
	aDRControlFlowGraph phiFunctions do: [ :phi |
		
		(phi operands reject:[ :op | op isNullValue ]) size = 1
			ifTrue: [ phi replaceByCopy ]
			ifFalse: [ self storePhiOperands: phi ] ]
]

{ #category : 'operations' }
DRLocalVariableInstructionScheluder >> shouldIgnore: aDRInstruction [

	^ aDRInstruction isStore or: [
		  aDRInstruction isPhiFunction and: [
			  aDRInstruction operands allSatisfy: [ :op | op isStore ] ] ]
]

{ #category : 'transformation' }
DRLocalVariableInstructionScheluder >> storePhiOperands: aDRPhiFunction [

	| variableName loadInst cfgScope |
	variableName := aDRPhiFunction result name asLowercase asDRValue.
	
	"add the new created temporary variable to the scope"
	cfgScope := aDRPhiFunction basicBlock controlFlowGraph scope.
	cfgScope addTemps: { aDRPhiFunction result name asLowercase }.
	
	loadInst := DRLoadTemporaryVariable 
			operands: { variableName }
			result: aDRPhiFunction result.
	loadInst vectorName: cfgScope tempVectorName.
	aDRPhiFunction replaceBy: loadInst.

	(aDRPhiFunction operands allSatisfy: [ :op |
		 op isStoreTemporaryVariable ]) ifTrue: [ "If they are already temporaries then it is ok"
		^ self ].

	(aDRPhiFunction operands copyWithout: aDRPhiFunction) do: [ :op |
		| storeInst |
		storeInst := DRStoreTemporaryVariable
			             operands: {
					             variableName.
					             op }
			             result: DRNoRegister new.
		storeInst vectorName: cfgScope tempVectorName.
		op basicBlock add: storeInst after: op ]
]

{ #category : 'transformation' }
DRLocalVariableInstructionScheluder >> storeResultsToBeReused: aDRControlFlowGraph [

	aDRControlFlowGraph instructionsDo: [ :i |
		(self shouldIgnore: i) ifFalse: [
			i users size > 1 ifTrue: [ self storeReusedResultOf: i ] ] ]
]

{ #category : 'transformation' }
DRLocalVariableInstructionScheluder >> storeReusedResultOf: aDRInstruction [

	| variableName storeInst |
	self assert: aDRInstruction result isRegister.

	variableName := aDRInstruction isLoadTemporaryVariable
		                ifTrue: [ aDRInstruction operand1 ]
		                ifFalse: [
		                aDRInstruction result name asLowercase asDRValue ].

	aDRInstruction users copy do: [ :user |
		| loadInst |
		loadInst := DRLoadTemporaryVariable 
				operands: { variableName }
				result: aDRInstruction controlFlowGraph allocateTemporaryRegister.
		user basicBlock add: loadInst before: user.
		user replaceDependency: aDRInstruction by: loadInst ].

	storeInst := DRStoreTemporaryVariable
		             operands: {
				             variableName.
				             aDRInstruction }
		             result: DRNoRegister new.
	aDRInstruction basicBlock add: storeInst after: aDRInstruction
]
