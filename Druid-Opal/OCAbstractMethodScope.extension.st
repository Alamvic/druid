Extension { #name : 'OCAbstractMethodScope' }

{ #category : '*Druid-Opal' }
OCAbstractMethodScope >> hasTempDefinitionFor: tempName [ 

	^ self tempVarNames includes: tempName 
]
