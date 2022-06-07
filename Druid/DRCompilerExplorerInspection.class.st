Class {
	#name : #DRCompilerExplorerInspection,
	#superclass : #StInspection,
	#instVars : [
		'cfgPresenter',
		'optimisationToolbar',
		'configToolbar'
	],
	#category : #'Druid-UI'
}

{ #category : #api }
DRCompilerExplorerInspection >> activateOnDoubleClick [

	cfgPresenter activateOnDoubleClick
]

{ #category : #accessing }
DRCompilerExplorerInspection >> blocks [
	
	^ model blocks
]

{ #category : #'instance creation' }
DRCompilerExplorerInspection >> configureCFGCanvas: view [

	model cfg showIn: view
]

{ #category : #api }
DRCompilerExplorerInspection >> hasOutputActivationPort [

	^ true
]

{ #category : #initialization }
DRCompilerExplorerInspection >> initializePresenters [

	self layout: (SpBoxLayout newVertical
			 add: (configToolbar := self newToolbar) expand: false;
			 add: (optimisationToolbar := self newToolbar) expand: false;
			 add: (cfgPresenter := self newCFGPresenter);
			 yourself).

	self addConfigButtons.
	self addOptimisationButtons
]

{ #category : #'instance creation' }
DRCompilerExplorerInspection >> newCFGPresenter [

	^ SpRoassalPresenter new
		script: [ :canvas | self configureCFGCanvas: canvas ];
		yourself
]

{ #category : #api }
DRCompilerExplorerInspection >> outputActivationPort [
	
	^ (SpActivationPort newPresenter: self)
		delegateTo: [ cfgPresenter ];
		yourself
]

{ #category : #initialization }
DRCompilerExplorerInspection >> refresh [

	"HACKSSSS"

	| oldBlock |
	oldBlock := cfgPresenter canvas inspectorContext presentation.
	cfgPresenter refresh.
	cfgPresenter whenActivatedDo: oldBlock
]
