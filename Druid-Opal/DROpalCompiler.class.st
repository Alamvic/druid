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

{ #category : 'as yet unclassified' }
DROpalCompiler class >> copyMethod: selector from: aClass to: newClass [

	| method |
	method := aClass >> selector.
	newClass addSelector: selector withMethod: method copy
]

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
	
	self recompileClass: aClass except: #()
]

{ #category : 'as yet unclassified' }
DROpalCompiler class >> recompileClass: aClass except: ignoredSelectors [
	
	| oldName _superclass slots packageName newName tag newClass n |
	oldName := aClass name.
	_superclass := aClass superclass.
	slots := aClass slots collect: [ :slot | slot name ].
	tag := aClass packageTag.
	packageName := aClass package name.
	newName := #DR , oldName.

	self removeClassIfNeeded: newName.

	newClass := _superclass << newName 
		slots: slots;
		tag: tag;
		package: packageName;
		install.
		
	n := 0.	
		
	(aClass selectors copyWithoutAll: ignoredSelectors)
	do: [ :sel |
		[ self recompile: sel from: aClass to: newClass ]
		on: Error do: [ :ex |
			n := n + 1.
			n trace. '. ' trace.
			sel traceCr.
			ex description traceCr.
			self copyMethod: sel from: aClass to: newClass
		]
	].

	ignoredSelectors do: [ :sel | 
		self copyMethod: sel from: aClass to: newClass ].
	
	aClass class selectors do: [ :sel |
		self copyMethod: sel from: aClass class to: newClass class ].
		
	^ newClass
]

{ #category : 'removing' }
DROpalCompiler class >> removeClassIfNeeded: newName [

	(Smalltalk systemNavigation environment classNamed: newName)
		ifNotNil: [ :class | Smalltalk systemNavigation removeClass: class ]
]
