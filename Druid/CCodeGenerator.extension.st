Extension { #name : #CCodeGenerator }

{ #category : #'*Druid' }
CCodeGenerator >> inferTypes [
	
	(SlangTyper on: self)
		stopOnErrors: self stopOnErrors;
		inferTypes
]

{ #category : #'*Druid' }
CCodeGenerator >> stopOnErrors [

	^ stopOnErrors
		ifNil: [ stopOnErrors := true ]
]
