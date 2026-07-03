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
	
	^ self recompile: selector from: oldClass to: oldClass 
]

{ #category : 'recompilation' }
DROpalCompiler class >> recompile: selector from: oldClass to: newClass [

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

	newSelector := "#dr ," selector.
	newMethod := self generateMethodFromCFG: cfg withSelector: newSelector numArgs: oldMethod numArgs.

	^ newClass addSelector: newSelector withMethod: newMethod 
]

{ #category : 'as yet unclassified' }
DROpalCompiler class >> recompileClass: aClass [ 
	
	| oldName _superclass slots packageName newName tag newClass |
	oldName := aClass name.
	_superclass := aClass superclass.
	slots := aClass slots collect: [ :slot | slot name ].
	tag := aClass packageTag.
	packageName := aClass package name.
	
	newName := #DR , oldName.
	newClass := _superclass << newName 
		slots: slots;
		tag: tag;
		package: packageName;
		install.
		
	aClass selectors do: [ :sel |
		self recompile: sel from: aClass to: newClass ].
		
	^ newClass
]
