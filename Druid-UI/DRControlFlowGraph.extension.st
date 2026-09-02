Extension { #name : 'DRControlFlowGraph' }

{ #category : '*Druid-UI' }
DRControlFlowGraph >> asInspectorIn: view [

	self configureVisualization: view.
	^ SpRoassal3InspectorPresenter new
		canvas: view;
		yourself
]

{ #category : '*Druid-UI' }
DRControlFlowGraph >> configureVisualization: view [

	| blockViews controller |
	blockViews := self blocks collect: [ :b | self createBlockView: b ].

	view addAll: blockViews.
	self showLines: blockViews in: view.

	"All the characters are displayed using a grid layout"
	RSSugiyamaLayout new
		horizontalGap: 50;
		applyOn: blockViews.

	self showInfoIn: view.

	controller := RSCanvasController new
		              noLegend;
		              yourself.

	view @ controller
]

{ #category : '*Druid-UI' }
DRControlFlowGraph >> createBlockView: aBlock [

	| box label boxes composite labelBackground borderColor |
	label := RSLabel new
		         text: aBlock id asString;
		         color: Color black;
		         fontSize: 4;
		         yourself.
	labelBackground := RSCircle new
		                   color: Color white;
		                   border: (RSBorder new color: 'black');
		                   radius: (label extent x max: label extent y) - 2;
		                   yourself.
	borderColor := (markedBlocks includes: aBlock)
		ifTrue: [ Color red ]
		ifFalse: [ Color black ].
	box := RSBox new
		       noPaint;
		       border: (RSBorder new
				        width: 2;
				        color: borderColor;
				        yourself);
		       cornerRadius: 2;
		       yourself.

	boxes := aBlock instructions
		         collect: [ :inst | self createinstructionView: inst ]
		         as: RSGroup.
	RSGridLayout on: boxes.

	box
		position: boxes position;
		extent: boxes extent + 10.

	composite := RSComposite new
		             add: box;
		             addAll: boxes;
		             draggable;
		             model: aBlock;
		             adjustToChildren;
		             @ (RSPopup text: [ :model |
					              String streamContents: [ :s |
							              (model instructions collect: #prettyPrint)
								              asStringOn: s
								              delimiter: String cr ] ]);
		             @ (RSMenuActivable new menuDo: [ :menu :element |
					              menu
						              add: 'Tail-duplicate'
						              target: element model
						              selector: #tailDuplicate ]);
		             yourself.

	RSLocation new
		outer;
		right;
		top;
		offset: -2 @ 2;
		move: label on: boxes.

	labelBackground position: label position.
	composite add: labelBackground.
	composite add: label.
	^ composite
]

{ #category : '*Druid-UI' }
DRControlFlowGraph >> createinstructionView: instruction [

	^ RSBox new
		  model: instruction;
		  color: (self instructionColor: instruction);
		  border: (RSBorder new
				   color: (instruction isStaged
						    ifTrue: [ Color green paler duller duller duller ]
						    ifFalse: [ Color black ]);
				   joinMiter);
		  @ (RSMenuActivable new menuDo: [ :menu :element |
				   menu
					   add: 'Inline' target: element model selector: #inline;
					   add: 'Unroll' target: element model selector: #uiUnroll ]);
		  yourself
]

{ #category : '*Druid-UI' }
DRControlFlowGraph >> inspectionCFG: aBuilder [

	<inspectorPresentationOrder: 0 title: 'CFG'>
	| view |
	view := RSCanvas new.
	^ self asInspectorIn: view
]

{ #category : '*Druid-UI' }
DRControlFlowGraph >> instructionColor: instruction [

	instruction isJump ifTrue: [
		instruction isBackJump ifTrue: [ ^ Color purple muchLighter ].
		instruction isConditionalBranch ifTrue: [ ^ Color purple ].
		^ Color purple muchDarker ].
	instruction isMandatoryInstruction ifTrue: [ ^ Color red ].
	instruction users isEmpty ifTrue: [ ^ Color gray ].
	instruction isPhiFunction ifTrue: [ ^ Color orange ].
	(instruction operands anySatisfy: [ :op | op isPhysicalRegister ])
		ifTrue: [ ^ Color red darker ].
	(instruction operands anySatisfy: [ :op | op isConstant ]) ifTrue: [
		^ Color yellow darker ].
	instruction isCopy ifTrue: [ ^ Color blue muchLighter ].

	^ Color white
]

{ #category : '*Druid-UI' }
DRControlFlowGraph >> showBranchConstraints: branchEdges [

	^ String streamContents: [ :s | 
		  branchEdges
			  do: [ :e | 
				  e constraints associationsDo: [ :assoc | 
					  s << (assoc key asString , ' ' , assoc value asString , String cr) ] ]
			  separatedBy: [ 
				  s << (String cr , '--o--o--o--o--o--o--o--' , String cr) ] ]
]

{ #category : '*Druid-UI' }
DRControlFlowGraph >> showInfoIn: view [

	| label instructions blocks paths info |
		
	label := [ :text | 
	         RSLabel new
		         text: text;
		         color: Color black;
		         fontSize: 14;
		         bold;
		         yourself ].


	blocks := label value: self blocks size asString , ' blocks'.
	instructions := label value:
		                self instructions size asString , ' instructions'.
	paths := label value: self pathsSize asString , ' paths'.
	info := { blocks . instructions .  paths }.

	RSVerticalLineLayout new
		alignLeft;
		on: info.
	info := info asShape.
	 
	RSLocation new
		below;
		offset:( 0 @ 20);
		move: info on: view.

	view add: info
]

{ #category : '*Druid-UI' }
DRControlFlowGraph >> showLines: blockViews in: view [

	| eb |
	eb := RSLineBuilder arrowedLine.
	eb canvas: view.
	eb color: Color blue muchDarker.
	eb width: 2.
	eb withBorderAttachPoint.
	eb shapes: blockViews.
	eb connectToAll: #successors.

	view lines pushBack.
	view lines do: [ :line |
		| source destination |
		source := line model key.
		destination := line model value.
		line model: (self edgesFrom: source to: destination).
		line @ (RSPopup text: [ :edgs | self showBranchConstraints: edgs ]).
		(self isAlmostDeadBranch: source to: destination) ifTrue: [
			line color: Color green muchDarker ].
		(self isDeadBranch: source to: destination) ifTrue: [
			line color: Color red muchDarker ] ]
]
