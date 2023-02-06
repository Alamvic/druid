Extension { #name : #VMMaker }

{ #category : #'*Druid' }
VMMaker >> stopOnErrors [

	^ stopOnErrors
		ifNil: [ stopOnErrors := true ]
]
