Extension { #name : 'OCAbstractMethodScope' }

{ #category : '*Druid' }
OCAbstractMethodScope >> hasTempDefinitionFor: tempName [ 

	^ self tempVarNames includes: tempName 
]
