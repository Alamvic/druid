Extension { #name : 'DRJump' }

{ #category : '*Druid-UI' }
DRJump >> inspectionSource: aBuilder [
	<inspectorPresentationOrder: 0 title: 'Source'>

	^ SpCodePresenter new
		text: target asString;
		yourself
]
