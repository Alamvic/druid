Class {
	#name : 'DRStoreVectorTemporaryVariable',
	#superclass : 'DRStoreTemporaryVariable',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'testing' }
DRStoreVectorTemporaryVariable >> isTempVectorTemp [ 

	^ true
]

{ #category : 'testing' }
DRStoreVectorTemporaryVariable >> name [ 

	^ self variableName 
]

{ #category : 'testing' }
DRStoreVectorTemporaryVariable >> scope: aScope [
	"need to fix, just to make it work"
]
