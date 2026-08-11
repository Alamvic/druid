Extension { #name : 'DRScope' }

{ #category : '*Druid-UI' }
DRScope >> inspectionInlines: aBuilder [
	<inspectorPresentationOrder: 10 title: 'Inlining Graph'>
	
	| view blockViews eb |
	view := RSCanvas new.
	blockViews := self withAllInlinedScopes asSet collect: [ :aScope | | g label |
		g := RSGroup new.
		(label := (RSLabel new text: aScope selector asString)
			borderColor: Color black;
			yourself).
		g add: (RSBox new
			color: (aScope hasMissingInlines ifTrue: [ Color red muchLighter ] ifFalse: [ Color white ]);
			border: (RSBorder new 
				color: (aScope node isMessageSend ifTrue: [ Color orange ] ifFalse: [ Color purple ]); 
				width: 2);
			extent: label extent + 10;
			yourself).
		g add: label.
		g asShape
			@ RSDraggable;
			model: aScope;
			yourself ] as: Array.
	view addAll: blockViews.
	
	eb := RSEdgeBuilder arrowedLine.
	eb canvas: view.
	eb withBorderAttachPoint.
	eb 	shapes: blockViews.
	eb connectToAll: #inlinedScopes.
	
	"All the characters are displayed using a grid layout"
	RSDominanceTreeLayout new
		horizontalGap: 50;
		shouldValidateCycles;
		applyOn: blockViews.
	view @ RSCanvasController.
	
	^ SpRoassalInspectorPresenter new
		canvas: view;
		yourself
]

{ #category : '*Druid-UI' }
DRScope >> inspectionSource: aBuilder [
	<inspectorPresentationOrder: 0 title: 'Source'>
	
	^ self node inspectionSource: aBuilder
]
