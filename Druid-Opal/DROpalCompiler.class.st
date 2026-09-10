Class {
	#name : 'DROpalCompiler',
	#superclass : 'Object',
	#category : 'Druid-Opal-Objects',
	#package : 'Druid-Opal',
	#tag : 'Objects'
}

{ #category : 'as yet unclassified' }
DROpalCompiler class >> copyMethod: selector from: aClass to: newClass [

	| method |
	method := aClass >> selector.	
	newClass compile: method ast formattedCode
]

{ #category : 'utilities' }
DROpalCompiler class >> generateMethodFromCFG: aDRControlFlowGraph withSelector: selector numArgs: numArgs [
	
	| bytecodeGenerator |	
	aDRControlFlowGraph applyOptimisation: DRFrameInfoCleaner new.
	aDRControlFlowGraph applyOptimisation: DRBranchCollapse new.
	
	aDRControlFlowGraph applyOptimisation: DRPhiSimplication new.
	aDRControlFlowGraph applyOptimisation: (DRSCCP then: DRDeadCodeElimination).

	DRUnconditionalBackJumpScheluder new applyTo: aDRControlFlowGraph.
	DRLocalVariableInstructionScheluder new applyTo: aDRControlFlowGraph.
	aDRControlFlowGraph applyOptimisation: DRCleanScopes new.
	aDRControlFlowGraph applyOptimisation: DRDeadCodeElimination new.

	bytecodeGenerator := DRBytecodeGenerator new.
	bytecodeGenerator numArgs: numArgs.
	bytecodeGenerator methodName: selector.
	bytecodeGenerator generateTargetASTFromIR: aDRControlFlowGraph methodName: selector.
	^ bytecodeGenerator targetAST
]

{ #category : 'as yet unclassified' }
DROpalCompiler class >> patchLiteralsIn: method from: oldPackage [
	
	"TODO: Does it patch the literals inside blocks???"

	| allGlobals |
	allGlobals := OrderedCollection new.
	method withAllNestedLiteralsDo: [ :each | 
		(each isVariableBinding and: [ each isGlobalVariable ]) ifTrue: [ 
			 allGlobals add: each ] ].
	
	allGlobals asSet do: [ :each |
					each value package = oldPackage ifTrue: [ | index newClass |
						index := method indexOfLiteral: each.
						newClass := Smalltalk classNamed: (#DR , each name).
						method literalAt: index put: newClass binding ] ]
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
	
	| oldName _superclass slots packageName newClassName newPackageName tag newClass n sharedVariables |
	oldName := aClass name.
	_superclass := aClass superclass.
	slots := aClass slots collect: [ :slot | slot name ].
	sharedVariables := aClass classVariables collect: [ :slot | slot name ].
	tag := aClass packageTag.
	packageName := aClass package name.

	newClassName := #DR , oldName.
	newPackageName := packageName , '-Druid'.

	self removeClassIfNeeded: newClassName.
	
	('Recompiling ' , oldName , ' into ' , newClassName , ' in ' , newPackageName) traceCr.

	newClass := _superclass << newClassName 
		slots: slots;
		tag: tag;
		sharedVariables: sharedVariables;
		package: newPackageName;
		install.
		
	"Copy class slots"		
	aClass class slots do: [ :slot | newClass class addSlot: slot copy ]. 
		
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
