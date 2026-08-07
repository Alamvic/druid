Extension { #name : 'DRBasicBlock' }

{ #category : '*Druid-UI' }
DRBasicBlock >> inspectionInstructions: aBuilder [
	<inspectorPresentationOrder: 0 title: 'Instructions'> 
	
	^ aBuilder newList
		display: [ :each | StObjectPrinter asTruncatedTextFrom: each prettyPrint ];
		items: self instructions;
		yourself
]

{ #category : '*Druid-UI' }
DRBasicBlock >> inspectionSourceCode: builder [
	<inspectorPresentationOrder: 30 title: 'Source code'>

	| sourceInterval |
	sourceInterval := self sourceInterval.
	^ SpCodePresenter new 
		beForBehavior: self methodNode methodClass;
		text: self source;
		addTextSegmentDecoration: (SpTextPresenterDecorator forHighlight
			interval: (sourceInterval first to: sourceInterval last + 1);
			yourself);
		yourself
]
