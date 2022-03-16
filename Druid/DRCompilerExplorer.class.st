Class {
	#name : #DRCompilerExplorer,
	#superclass : #Object,
	#instVars : [
		'method',
		'interpreter',
		'interpreterClass',
		'selector',
		'cfg'
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
DRCompilerExplorer >> apply: aClass [ 
	
	aClass new applyTo: self cfg
]

{ #category : #accessing }
DRCompilerExplorer >> blocks [
	
	^ self cfg blocks
]

{ #category : #'as yet unclassified' }
DRCompilerExplorer >> cfg [

	cfg ifNotNil: [ ^ cfg ].
	^ cfg := DRPrimitiveCompilerCompiler new
		primitiveName: self selector;
		interpreter: self interpreter;
		generateDruidIR
]

{ #category : #'as yet unclassified' }
DRCompilerExplorer >> inspectionCFG: aBuilder [
	<inspectorPresentationOrder: 0 title: 'CFG'> 
	
	^ DRCompilerExplorerInspection on: self
]

{ #category : #'as yet unclassified' }
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

{ #category : #'as yet unclassified' }
DRCompilerExplorer >> method [

	^ self interpreterClass lookupSelector: selector
]

{ #category : #'as yet unclassified' }
DRCompilerExplorer >> methodClass [

	^ self interpreter class
]

{ #category : #'as yet unclassified' }
DRCompilerExplorer >> methodSource [

	^ self method sourceCode
]

{ #category : #accessing }
DRCompilerExplorer >> optimizations [
	
	^ { DRDeadBlockElimination . DRSCCP . DRCopyPropagation . DRDeadCodeElimination . DRCleanControlFlow . DRPhiSimplication. DRDeadPathElimination }
]

{ #category : #'as yet unclassified' }
DRCompilerExplorer >> selector [

	^ selector
]

{ #category : #'as yet unclassified' }
DRCompilerExplorer >> selector: aSelector [

	selector := aSelector
]
