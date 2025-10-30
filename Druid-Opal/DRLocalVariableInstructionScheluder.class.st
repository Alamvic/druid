Class {
	#name : 'DRLocalVariableInstructionScheluder',
	#superclass : 'DROptimisation',
	#category : 'Druid-Opal-Optimizations',
	#package : 'Druid-Opal',
	#tag : 'Optimizations'
}

{ #category : 'operations' }
DRLocalVariableInstructionScheluder >> applyTo: cfg [

	self replacePhisByStores: cfg.
	self storeResultsToBeReused: cfg.
	self cleanRedundantStores: cfg
]

{ #category : 'as yet unclassified' }
DRLocalVariableInstructionScheluder >> cleanRedundantStores: cfg [

	cfg instructions copy
		select: [ :i | i isStoreTemporaryVariable ]
		thenDo: [ :store |
			(store storeValue isInstruction and: [
				 store storeValue isLoadTemporaryVariable ]) ifTrue: [
				store address = store storeValue address ifTrue: [
					store scope = store storeValue scope ifTrue: [
						store removeFromCFG ] ] ] ]
]

{ #category : 'transformation' }
DRLocalVariableInstructionScheluder >> replacePhisByStores: aDRControlFlowGraph [
	
	aDRControlFlowGraph phiFunctions do: [ :phi |
		
		phi operands size = 1
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

	| variableName loadInst cfg|
	variableName := aDRPhiFunction result name asLowercase asDRValue.
	
	"add the new created temporary variable to the scope"
	cfg := aDRPhiFunction controlFlowGraph.
	cfg addTempNameToScope: aDRPhiFunction result name asLowercase.
	
	loadInst := DRLoadTemporaryVariable 
			operands: { variableName }
			result: aDRPhiFunction result.
	cfg addScopeToInstructionIfNeeded: loadInst .
	aDRPhiFunction replaceBy: loadInst.

	(aDRPhiFunction operandsWithoutMe allSatisfy: [ :op |
		 op result isNoResult  ]) ifTrue: [ "If they are already temporaries then it is ok"
		^ self ].

	aDRPhiFunction operandsWithoutMe 
		select: [ :op | op isInstruction ]
		thenDo: [ :op |
		| storeInst |
		storeInst := DRStoreTemporaryVariable
			             operands: {
					             variableName.
					             op }
			             result: DRNoRegister new.
		
		cfg addScopeToInstructionIfNeeded: storeInst .
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

    | variableName storeInst cfg |

	"If it is a Load then the variable is already stored (can be reused)"
	aDRInstruction isLoadTemporaryVariable ifTrue: [ ^ self ].

    self assert: aDRInstruction result isRegister.
    cfg := aDRInstruction controlFlowGraph.

    variableName := aDRInstruction isLoadTemporaryVariable
                        ifTrue: [ aDRInstruction operand1 ]
                        ifFalse: [
                        aDRInstruction result name asLowercase asDRValue ].

    cfg addTempNameToScope: variableName value.

    aDRInstruction users copy do: [ :user |
        | loadInst |
        loadInst := DRLoadTemporaryVariable 
                operands: { variableName }
                result: aDRInstruction controlFlowGraph allocateTemporaryRegister.
        aDRInstruction controlFlowGraph addScopeToInstructionIfNeeded: loadInst.
        user basicBlock add: loadInst before: user.
        user replaceDependency: aDRInstruction by: loadInst ].

    storeInst := DRStoreTemporaryVariable
                     operands: {
                             variableName.
                             aDRInstruction }
                     result: DRNoRegister new.

    cfg addScopeToInstructionIfNeeded: storeInst.
    aDRInstruction basicBlock add: storeInst after: aDRInstruction
]
