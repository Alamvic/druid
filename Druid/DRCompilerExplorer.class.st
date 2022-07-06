Class {
	#name : #DRCompilerExplorer,
	#superclass : #Object,
	#instVars : [
		'interpreter',
		'interpreterClass',
		'selector',
		'cfg',
		'previousCfgs',
		'isRecording',
		'shouldGeneratePaths',
		'lastOpt'
	],
	#classInstVars : [
		'experiments'
	],
	#category : #'Druid-UI'
}

{ #category : #accessing }
DRCompilerExplorer class >> experiments [

	^ experiments
]

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

{ #category : #'as yet unclassified' }
DRCompilerExplorer class >> saveNewExperiment: experiment [

	experiments add: experiment
]

{ #category : #optimising }
DRCompilerExplorer >> apply: opt [

	isRecording ifTrue: [ previousCfgs add: lastOpt -> self cfg veryDeepCopy ].
	self cfg applyOptimisation: opt.
	lastOpt := opt.
	shouldGeneratePaths ifTrue: [ self cfg generatePaths ]
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

	cfg := previousCfgs removeLast value
]

{ #category : #initialization }
DRCompilerExplorer >> initialize [

	super initialize.
	previousCfgs := OrderedCollection new.
	isRecording := false.
	shouldGeneratePaths := false
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

	^ interpreterClass ifNil: [ DruidTestInterpreter ]
]

{ #category : #accessing }
DRCompilerExplorer >> interpreterClass: anObject [

	interpreterClass := anObject
]

{ #category : #accessing }
DRCompilerExplorer >> isRecording [

	^ isRecording
]

{ #category : #accessing }
DRCompilerExplorer >> isRecording: anObject [

	isRecording := anObject
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
		  DRBranchCollapse.
		  DRDeadBlockElimination.
		  DRCleanControlFlow.
		  DRSCCP.
		  DRGlobalValueNumbering.
		  DRCopyPropagation.
		  DRRedundantCopyElimination.
		  DRPhiSimplication.
		  DRDeadCodeElimination.
		  DRFailureCodeBasedTailDuplication.
		  DRDeadEdgeSplitting.
		  DRDeadBranchElimination }
]

{ #category : #accessing }
DRCompilerExplorer >> previousCfgs [

	^ previousCfgs
]

{ #category : #optimising }
DRCompilerExplorer >> saveExperiment [

	self class saveNewExperiment: (DROptimisationExperiment new
			 timestamp: DateAndTime now;
			 movie: previousCfgs;
			 yourself)
]

{ #category : #accessing }
DRCompilerExplorer >> selector [

	^ selector
]

{ #category : #accessing }
DRCompilerExplorer >> selector: aSelector [

	selector := aSelector
]

{ #category : #accessing }
DRCompilerExplorer >> shouldGeneratePaths [

	^ shouldGeneratePaths
]

{ #category : #accessing }
DRCompilerExplorer >> shouldGeneratePaths: anObject [

	shouldGeneratePaths := anObject
]
