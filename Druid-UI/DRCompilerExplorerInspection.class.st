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

{ #category : #adding }
DRCompilerExplorerInspection >> addConfigButtons [

	configToolbar addItem: (self newToolbarToggleButton
			 label: 'Recording';
			 state: model isRecording;
			 whenChangedDo: [ :state | model isRecording: state ];
			 yourself).

	configToolbar addItem: (self newToolbarToggleButton
			 label: 'Paths';
			 state: model shouldGeneratePaths;
			 whenChangedDo: [ :state | model shouldGeneratePaths: state ];
			 yourself).

	configToolbar addItem: (self newToolbarButton
			 label: 'Optimisation: ' , model previousCfgs size asString;
			 action: [ model previousCfgs inspect ];
			 yourself).

	configToolbar addItem: (self newToolbarButton
			 label: 'Save!';
			 action: [ model saveExperiment ];
			 yourself)
]

{ #category : #adding }
DRCompilerExplorerInspection >> addOptimisationButtons [

	optimisationToolbar addItem: (self newToolbarButton
			 label: '< Back';
			 action: [ 
				 model goBack.
				 self refresh ];
			 yourself).

	model optimizations do: [ :opt | 
		| toolbarButtom |
		toolbarButtom := self newToolbarButton.
		toolbarButtom label: opt name.
		toolbarButtom action: [ 
			model apply: opt new.
			self refresh ].
		optimisationToolbar addItem: toolbarButtom ]
]

{ #category : #accessing }
DRCompilerExplorerInspection >> blocks [
	
	^ model blocks
]

{ #category : #'instance creation' }
DRCompilerExplorerInspection >> configureCFGCanvas: view [

	model cfg showIn: view.

	view shapes select: #isLine thenDo: [ :line | 
		(line model includesAny: model path) ifTrue: [ 
			self highlightLine: line ] ]
]

{ #category : #api }
DRCompilerExplorerInspection >> hasOutputActivationPort [

	^ true
]

{ #category : #'instance creation' }
DRCompilerExplorerInspection >> highlightLine: line [

	line border: (RSBorder new
			 width: 3;
			 color: Color yellow darker;
			 yourself)
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
