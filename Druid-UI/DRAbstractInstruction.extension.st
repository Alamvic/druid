Extension { #name : 'DRAbstractInstruction' }

{ #category : '*Druid-UI' }
DRAbstractInstruction >> inspectionDependencies: aBuilder [
	<inspectorPresentationOrder: 10 title: 'Dependency Graph'>
	
	| view blockViews eb |
	view := RSCanvas new.
	blockViews := self withAllDependencies asSet collect: [ :c | | g label |
		g := RSGroup new.
		(label := (RSLabel new text: c label asString)
			borderColor: Color black;
			yourself).
		g add: (RSBox new
			color: Color white;
			border: (RSBorder new color: Color purple; width: 2);
			extent: label extent + 10;
			yourself).
		g add: label.
		g asShape
			@ RSDraggable;
			model: c;
			yourself ] as: Array.
	view addAll: blockViews.
	
	eb := RSEdgeBuilder arrowedLine.
	eb canvas: view.
	eb withBorderAttachPoint.
	eb 	shapes: blockViews.
	eb connectToAll: #dependencies.
	
	"All the characters are displayed using a grid layout"
	RSDominanceTreeLayout new
		horizontalGap: 50;
		shouldValidateCycles;
		applyOn: blockViews.
	view @ RSCanvasController.
	
	^ SpRoassal3InspectorPresenter new
		canvas: view;
		yourself
]

{ #category : '*Druid-UI' }
DRAbstractInstruction >> inspectionSource: aBuilder [
	<inspectorPresentationOrder: 0 title: 'Source'>
	
	^ self originAST inspectionSource: aBuilder
]

{ #category : '*Druid-UI' }
DRAbstractInstruction >> inspectionUsers: aBuilder [
	<inspectorPresentationOrder: 10 title: 'Users Graph'>
	
	| view blockViews eb |
	view := RSCanvas new.
	blockViews := self withAllUsers asSet collect: [ :c | | g label |
		g := RSGroup new.
		(label := (RSLabel new text: c label asString)
			borderColor: Color black;
			yourself).
		g add: (RSBox new
			color: Color white;
			border: (RSBorder new color: Color purple; width: 2);
			extent: label extent + 10;
			yourself).
		g add: label.
		g asShape
			@ RSDraggable;
			model: c;
			yourself ] as: Array.
	view addAll: blockViews.
	
	eb := RSEdgeBuilder arrowedLine.
	eb canvas: view.
	eb withBorderAttachPoint.
	eb 	shapes: blockViews.
	eb connectToAll: #users.
	
	"All the characters are displayed using a grid layout"
	RSDominanceTreeLayout new
		horizontalGap: 50;
		shouldValidateCycles;
		applyOn: blockViews.
	view @ RSCanvasController.
	
	^ SpRoassal3InspectorPresenter new
		canvas: view;
		yourself
]

{ #category : '*Druid-UI' }
DRAbstractInstruction >> uiUnroll [

	| unrollingFactor |
	self isBackJump ifFalse: [ 
		^ self inform: 'Can only perform loop unrolling from back jumps' ].
	unrollingFactor := UIManager default
		                   request: 'Unrolling factor:'
		                   initialAnswer: '2'.
	(unrollingFactor ifNil: [ '' ]) ifEmpty: [ unrollingFactor := 2 ].
	self loop unroll: unrollingFactor asNumber
]
