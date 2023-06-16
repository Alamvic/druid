Extension { #name : #Boolean }

{ #category : #'*Druid' }
Boolean >> matchArgumentsAgainst: aPragma [

	^ aPragma arguments anySatisfy: [ : arg | self = arg ]
]

{ #category : #'*Druid' }
Boolean >> sccpEquals: aBoolean [

	self = aBoolean
]
