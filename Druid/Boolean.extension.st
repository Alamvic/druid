Extension { #name : 'Boolean' }

{ #category : '*Druid' }
Boolean >> asBoolean [

	^ self
]

{ #category : '*Druid' }
Boolean >> isBooleanValue [

	^ true
]

{ #category : '*Druid' }
Boolean >> matchArgumentsAgainst: aPragma [

	^ aPragma arguments anySatisfy: [ : arg | self = arg ]
]
