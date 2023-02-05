Extension { #name : #VMClass }

{ #category : #'*Druid' }
VMClass class >> resetInitializationOptions [

	super release.
	InitializationOptions := nil
]
