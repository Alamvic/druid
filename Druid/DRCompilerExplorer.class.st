Class {
	#name : #DRCompilerExplorer,
	#superclass : #Object,
	#instVars : [
		'method',
		'interpreter',
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
		interpreter: interpreter;
		generateDruidIR
]

{ #category : #initialization }
DRCompilerExplorer >> initialize [

	| theMemory |
	super initialize.
	
	theMemory := Spur64BitCoMemoryManager simulatorClass new.
	CogVMSimulatorLSB
			initializeWithOptions: {
				#BytesPerWord. 8.
				#ObjectMemory. theMemory class name.
				#bytecodeTableInitializer. #initializeBytecodeTableForSistaV1}
				objectMemoryClass: theMemory class.
	interpreter := CogVMSimulatorLSB basicNew
		objectMemory: theMemory;
		basicInitialize.
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

{ #category : #'as yet unclassified' }
DRCompilerExplorer >> method [

	^ interpreter class lookupSelector: selector
]

{ #category : #'as yet unclassified' }
DRCompilerExplorer >> methodClass [

	^ interpreter class
]

{ #category : #'as yet unclassified' }
DRCompilerExplorer >> methodSource [

	^ self method sourceCode
]

{ #category : #accessing }
DRCompilerExplorer >> optimizations [
	
	^ { DRDeadBlockElimination . DRSCCP . DRCopyPropagation . DRDeadCodeElimination . DRCleanControlFlow . DRPhiSimplication}
]

{ #category : #'as yet unclassified' }
DRCompilerExplorer >> selector [

	^ selector
]

{ #category : #'as yet unclassified' }
DRCompilerExplorer >> selector: aSelector [

	selector := aSelector
]
