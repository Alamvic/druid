Class {
	#name : #DRCompilerExplorerInspection,
	#superclass : #StInspection,
	#instVars : [
		'cfgPresenter',
		'toolbar'
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
		add: (toolbar := self newToolbar) expand: false;
		add: (cfgPresenter := self newCFGPresenter);
		yourself).
	model optimizations do: [ :opt |
		| toolbarButtom |
		toolbarButtom := self newToolbarButton.
		toolbarButtom label: opt name.
		toolbarButtom action: [ | oldBlock |
			model apply: opt.
			"HACKSSSS"
			oldBlock := cfgPresenter canvas inspectorContext presentation.
			cfgPresenter refresh.
			cfgPresenter whenActivatedDo: oldBlock ].
		toolbar addItem: toolbarButtom.
	]
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
