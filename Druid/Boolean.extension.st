Extension { #name : #Boolean }

{ #category : #'*Druid' }
Boolean >> matchArgumentsAgainst: aPragma [

	^ aPragma arguments anySatisfy: [ : arg | self = arg ]
]
