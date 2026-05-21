Class {
	#name : 'DROpalCompiler',
	#superclass : 'Object',
	#classInstVars : [
		'bytecodeGenerator'
	],
	#category : 'Druid-Opal-Objects',
	#package : 'Druid-Opal',
	#tag : 'Objects'
}

{ #category : 'utilities' }
DROpalCompiler class >> generateMethodFromCFG: aDRControlFlowGraph withSelector: selector numArgs: numArgs [
	
	aDRControlFlowGraph applyOptimisation: DRFrameInfoCleaner new.
	aDRControlFlowGraph applyOptimisation: DRBranchCollapse new.
	
	aDRControlFlowGraph applyOptimisation: DRPhiSimplication new.
	aDRControlFlowGraph applyOptimisation: (DRSCCP then: DRDeadCodeElimination).

	DRUnconditionalBackJumpScheluder new applyTo: aDRControlFlowGraph.
	DRLocalVariableInstructionScheluder new applyTo: aDRControlFlowGraph.
	aDRControlFlowGraph applyOptimisation: DRCleanScopes new.
	aDRControlFlowGraph applyOptimisation: DRDeadCodeElimination new.

	bytecodeGenerator numArgs: numArgs.
	bytecodeGenerator methodName: selector.
	bytecodeGenerator generateTargetASTFromIR: aDRControlFlowGraph methodName: selector.
	^ bytecodeGenerator targetAST
]

{ #category : 'recompilation' }
DROpalCompiler class >> recompile: selector from: oldClass [

	| compilerCompiler oldMethod cfg newMethod typeSystem newSelector |
	compilerCompiler := DRMethodCompilerCompiler new.
	typeSystem := DRCustomisationTypeSystem new.
	bytecodeGenerator := DRBytecodeGenerator new.

	oldMethod := oldClass >> selector.
	compilerCompiler configureForCompilerClass: nil.
	compilerCompiler interpreter: oldClass basicNew.
	compilerCompiler irGenerator typeSystem: typeSystem.

	cfg := compilerCompiler generateDruidIRFor: oldMethod.
	compilerCompiler optimize: cfg.
	compilerCompiler optimize: cfg.

	newSelector := #dr , selector.
	newMethod := self generateMethodFromCFG: cfg withSelector: newSelector numArgs: oldMethod numArgs.

	oldClass addSelector: newSelector withMethod: newMethod 
]
