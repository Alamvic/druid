Extension { #name : 'DRDominatorTree' }

{ #category : '*Druid-UI' }
DRDominatorTree >> asInspectorIn: view [

	self configureVisualization: view.
	^ SpRoassal3InspectorPresenter new
		canvas: view;
		yourself
]

{ #category : '*Druid-UI' }
DRDominatorTree >> configureVisualization: view [

	| blockViews controller |
	blockViews := self blockDictionary values collect: [ :treeNode | self createBlockView: treeNode ].

	view addAll: blockViews.
	self showLines: blockViews in: view.

	"All the characters are displayed using a grid layout"
	RSSugiyamaLayout new
		horizontalGap: 50;
		applyOn: blockViews.

	controller := RSCanvasController new
		noLegend;
		yourself.

	view @ controller
]

{ #category : '*Druid-UI' }
DRDominatorTree >> createBlockView: treeNode [

	| box label composite labelBackground |
	label := RSLabel new
		         text: treeNode block id asString;
		         color: Color black;
		         fontSize: 4;
		         yourself.
	labelBackground := RSEllipse new
		                   color: Color white;
		                   border: (RSBorder new color: 'black');
		                   extent: label extent + 5;
		                   yourself.
	box := RSBox new
		       noPaint;
		       border: (RSBorder new
				        width: 2;
				        color: Color black;
				        yourself);
		       cornerRadius: 2;
		       yourself.

	"	box
		position: boxes position;
		extent: boxes extent + 10."
	composite := { box } asShape
		             draggable;
		             model: treeNode;
		             yourself. ",boxes"

	RSLocation new
		outer;
		right;
		top;
		offset: 3 @ -3. ";
		move: label on: boxes"

	labelBackground position: label position.
	composite add: labelBackground.
	composite add: label.
	^ composite
]

{ #category : '*Druid-UI' }
DRDominatorTree >> inspectionDominatorTree [

	<inspectorPresentationOrder: 0 title: 'DominatorTree'>
	| view |
	view := RSCanvas new.
	^ self asInspectorIn: view
]

{ #category : '*Druid-UI' }
DRDominatorTree >> showLines: blockViews in: view [

	| eb |
	eb := RSLineBuilder arrowedLine.
	eb canvas: view.
	eb color: Color blue muchDarker.
	eb width: 2.
	eb withBorderAttachPoint.
	eb shapes: blockViews.
	eb connectToAll: [ :e | e children ].

	view lines pushBack.
	view lines do: [ :line |
		| source destination |
		source := line model key.
		destination := line model value ]
]
