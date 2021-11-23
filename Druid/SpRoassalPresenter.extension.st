Extension { #name : #SpRoassalPresenter }

{ #category : #'*Druid' }
SpRoassalPresenter >> defaultOutputPort [

	^ self outputSelectionPort
]

{ #category : #'*Druid' }
SpRoassalPresenter >> outputActivationPort [

	^ SpActivationPort newPresenter: self
]

{ #category : #'*Druid' }
SpRoassalPresenter >> outputSelectionPort [

	^ SpActivationPort newPresenter: self
]

{ #category : #'*Druid' }
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
