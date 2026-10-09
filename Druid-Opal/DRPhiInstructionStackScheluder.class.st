Class {
	#name : 'DRPhiInstructionStackScheluder',
	#superclass : 'DROptimisation',
	#category : 'Druid-Opal-Optimizations',
	#package : 'Druid-Opal',
	#tag : 'Optimizations'
}

{ #category : 'operations' }
DRPhiInstructionStackScheluder >> applyTo: cfg [

	self replacePhisByStores: cfg.
	self pushNonLocalVarsInPredecessors: cfg
]

{ #category : 'as yet unclassified' }
DRPhiInstructionStackScheluder >> cleanRedundantStores: cfg [

	cfg instructions copy
		select: [ :i | i isStoreTemporaryVariable ]
		thenDo: [ :store |
			(store storeValue isInstruction and: [
				 store storeValue isLoadTemporaryVariable ]) ifTrue: [
				store address = store storeValue address ifTrue: [
					store scope = store storeValue scope ifTrue: [
						store removeFromCFG ] ] ] ]
]

{ #category : 'instance creation' }
DRPhiInstructionStackScheluder >> newUniqueTempName [
	
	^ UUID new asString
]

{ #category : 'operations' }
DRPhiInstructionStackScheluder >> pushNonLocalVarsInPredecessors: cfg [

	cfg blocksInReversePostOrderDo: [ :bb | 
		"For each dependency, put it in a temp, and consume it locally as a temp read"
		bb dependencies do: [ :dep |
			| tempName store load localUsers |
			localUsers := (dep users select: [ :u | u basicBlock = bb ])
				sorted: [:a :b | a indexInBasicBlock < b indexInBasicBlock ].
			localUsers ifNotEmpty: [
				tempName := self newUniqueTempName.
				cfg addTempNameToScope: tempName.
				store := DRBCStoreTemporaryVariable
	                     operands: {
	                             tempName.
	                             dep value }
	                     result: DRNoRegister new.
				cfg addScopeToInstructionIfNeeded: store.
				dep addNext: store.
				load := DRBCLoadTemporaryVariable 
	                     operands: { tempName }
	                     result: cfg instructionFactory allocateTemporaryRegister.
				cfg addScopeToInstructionIfNeeded: load.

				localUsers first addBefore: load.
				localUsers do: [ :localUser | 
					localUser replaceDependency: dep by: load ] ]] ]
]

{ #category : 'transformation' }
DRPhiInstructionStackScheluder >> replacePhisByStores: aDRControlFlowGraph [

	aDRControlFlowGraph phiFunctions do: [ :phi |
		phi operands size = 1
			ifTrue: [ phi replaceByCopy ]
			ifFalse: [ self resolvePhiFunction: phi ] ]
]

{ #category : 'transformation' }
DRPhiInstructionStackScheluder >> resolvePhiFunction: aDRPhiFunction [

	"We move out from SSA representation to a stack representation.
	The operands are pushed as last instructions of the predecessors,
	 and the phi is replaced by a pop."

	| popInstruction |
	aDRPhiFunction hasUsers ifFalse: [ ^ self ].

	aDRPhiFunction operandsAndPredecessorsDo: [ :op :pred | pred push: op ].
	popInstruction := aDRPhiFunction basicBlock instructionFactory popIntoVariable.
	aDRPhiFunction replaceBy: popInstruction
]

{ #category : 'operations' }
DRPhiInstructionStackScheluder >> shouldIgnore: aDRInstruction [

	^ aDRInstruction isStore or: [
		  aDRInstruction isPhiFunction and: [
			  aDRInstruction operands allSatisfy: [ :op | op isStore ] ] ]
]

{ #category : 'transformation' }
DRPhiInstructionStackScheluder >> storeResultsToBeReused: aDRControlFlowGraph [

	aDRControlFlowGraph instructionsDo: [ :i |
		(self shouldIgnore: i) ifFalse: [
			i users size > 1 ifTrue: [ self storeReusedResultOf: i ] ] ]
]

{ #category : 'transformation' }
DRPhiInstructionStackScheluder >> storeReusedResultOf: aDRInstruction [

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
        loadInst := DRBCLoadTemporaryVariable 
                operands: { variableName }
                result: aDRInstruction controlFlowGraph allocateTemporaryRegister.
        aDRInstruction controlFlowGraph addScopeToInstructionIfNeeded: loadInst.
        user basicBlock add: loadInst before: user.
        user replaceDependency: aDRInstruction by: loadInst ].

    storeInst := DRBCStoreTemporaryVariable
                     operands: {
                             variableName.
                             aDRInstruction }
                     result: DRNoRegister new.

    cfg addScopeToInstructionIfNeeded: storeInst.
    aDRInstruction basicBlock add: storeInst after: aDRInstruction
]
