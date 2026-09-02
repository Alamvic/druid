Extension { #name : 'DRReturn' }

{ #category : '*Druid-UI' }
DRReturn >> inspectionSource: aBuilder [
	<inspectorPresentationOrder: 0 title: 'Source'>

	^ SpCodePresenter new
		text: result asString;
		yourself
]
