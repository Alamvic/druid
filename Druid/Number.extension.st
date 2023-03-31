Extension { #name : #Number }

{ #category : #'*Druid' }
Number >> expression [
	
	^ self asString
]

{ #category : #'*Druid' }
Number >> isJitCompileTimeExpression [
	
	^ true
]

{ #category : #'*Druid' }
Number >> matchArgumentsAgainst: aPragma [

	^ aPragma arguments anyOne = self
]
