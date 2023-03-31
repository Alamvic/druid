Extension { #name : #Symbol }

{ #category : #'*Druid' }
Symbol >> matchArgumentsAgainst: aPragma [

	aPragma
		argumentNamed: self
		ifNone: [ ^ false ].
	^ true
]

{ #category : #'*Druid' }
Symbol >> prettyPrint [
	
	^ self
]
