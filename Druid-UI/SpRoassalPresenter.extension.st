Extension { #name : 'SpRoassalPresenter' }

{ #category : '*Druid-UI' }
SpRoassalPresenter >> defaultOutputPort [

	^ self outputSelectionPort
]

{ #category : '*Druid-UI' }
SpRoassalPresenter >> outputActivationPort [

	^ SpActivationPort newPresenter: self
]

{ #category : '*Druid-UI' }
SpRoassalPresenter >> outputSelectionPort [

	^ SpActivationPort newPresenter: self
]

{ #category : '*Druid-UI' }
SpRoassalPresenter >> whenActivatedDo: aBlock [

	| context |
	context := self canvas inspectorContext.
	context presentation: aBlock.
	"self canvas morph setTakeKeyboardFocus."
	self canvas shapes @ context.
	self canvas
		when: RSShapeAddedEvent
		do: [ :evt | evt shape @ context ].
]
