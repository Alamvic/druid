Class {
	#name : #DRCompilerExplorer,
	#superclass : #Object,
	#instVars : [
		'method',
		'interpreter',
		'interpreterClass',
		'selector',
		'cfg',
		'previousCfgs'
	],
	#category : #'Druid-UI'
}

{ #category : #'instance creation' }
DRCompilerExplorer class >> on: aSelector [

	^ self new
		selector: aSelector;
		yourself
]

{ #category : #'instance creation' }
DRCompilerExplorer class >> onSelector: aSelector inClass: aClass [

	^ self new
		selector: aSelector;
		interpreterClass: aClass;
		yourself
]

{ #category : #optimising }
DRCompilerExplorer >> apply: opt [

	previousCfgs add: self cfg veryDeepCopy.
	self cfg applyOptimisation: opt
]

{ #category : #accessing }
DRCompilerExplorer >> blocks [
	
	^ self cfg blocks
]

{ #category : #accessing }
DRCompilerExplorer >> cfg [

	cfg ifNotNil: [ ^ cfg ].
	^ cfg := DRPrimitiveCompilerCompiler new
		primitiveName: self selector;
		interpreter: self interpreter;
		generateDruidIR
]

{ #category : #optimising }
DRCompilerExplorer >> goBack [

	cfg := previousCfgs removeLast
]

{ #category : #initialization }
DRCompilerExplorer >> initialize [

	super initialize.
	previousCfgs := OrderedCollection new
]

{ #category : #inspecting }
DRCompilerExplorer >> inspectionCFG: aBuilder [
	<inspectorPresentationOrder: 0 title: 'CFG'> 
	
	^ DRCompilerExplorerInspection on: self
]

{ #category : #inspecting }
DRCompilerExplorer >> inspectionSource [
	<inspectorPresentationOrder: 30 title: 'Source'>
	
	^ SpCodePresenter new 
		beForBehavior: self methodClass;
		text: self methodSource;
		yourself
]

{ #category : #initialization }
DRCompilerExplorer >> interpreter [

	| theMemory |
	interpreter ifNotNil: [ ^ interpreter ].

	theMemory := Spur64BitCoMemoryManager simulatorClass new.
	self interpreterClass
		initializeWithOptions: { 
				#BytesPerWord.
				8.
				#ObjectMemory.
				theMemory class name.
				#bytecodeTableInitializer.
				#initializeBytecodeTableForSistaV1 }
		objectMemoryClass: theMemory class.
	^ interpreter := self interpreterClass basicNew
		                 objectMemory: theMemory;
		                 basicInitialize
]

{ #category : #accessing }
DRCompilerExplorer >> interpreterClass [

	^ interpreterClass ifNil: [ CogVMSimulatorLSB ]
]

{ #category : #accessing }
DRCompilerExplorer >> interpreterClass: anObject [

	interpreterClass := anObject
]

{ #category : #accessing }
DRCompilerExplorer >> method [

	^ self interpreterClass lookupSelector: selector
]

{ #category : #accessing }
DRCompilerExplorer >> methodClass [

	^ self interpreter class
]

{ #category : #accessing }
DRCompilerExplorer >> methodSource [

	^ self method sourceCode
]

{ #category : #accessing }
DRCompilerExplorer >> optimizations [

	^ { 
		  DRDeadBlockElimination.
		  DRDeadCodeElimination.
		  DRCleanControlFlow.
		  DRSCCP.
		  DRGlobalValueNumbering.
		  DRCopyPropagation.
		  DRRedundantCopyElimination.
		  DRPhiSimplication.
		  DRFailureCodeBasedTailDuplication.
		  DRDeadPathSplitting.
		  DRDeadPathElimination }
]

{ #category : #accessing }
DRCompilerExplorer >> selector [

	^ selector
]

{ #category : #accessing }
DRCompilerExplorer >> selector: aSelector [

	selector := aSelector
]
