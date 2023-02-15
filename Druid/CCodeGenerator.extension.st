Extension { #name : #CCodeGenerator }

{ #category : #'*Druid' }
CCodeGenerator >> stopOnErrors [

	^ stopOnErrors
		ifNil: [ stopOnErrors := true ]
]
