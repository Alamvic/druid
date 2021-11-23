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

	| blockViews eb |
	blockViews := self blocks collect: [ :c | | g |
		g := RSGroup new.
		g add: (RSBox new
			size: 50;
			color: Color veryVeryLightGray;
			yourself).
		g add: (RSLabel new text: c id asString).
		g asShape
			@ RSDraggable;
			@ (RSMenuActivable new menuDo: [ :menu :element | 
				menu
					add: 'Tail-duplicate'
					target: element model
					selector: #tailDuplicate ]);
			model: c;
			yourself ].
	view addAll: blockViews.
	
	eb := RSEdgeBuilder arrowedLine.
	eb canvas: view.
	eb withBorderAttachPoint.
	eb 	shapes: blockViews.
	eb connectToAll: #successors.
	
	"All the characters are displayed using a grid layout"
	RSSugiyamaLayout new
		horizontalGap: 50;
		applyOn: blockViews.
	view @ RSCanvasController.
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
