Class {
	#name : 'DRUnsignedRightShift',
	#superclass : 'DRSignedRightShift',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRUnsignedRightShift >> acceptVisitor: aVisitor [
	
	^ aVisitor visitUnsignedRightShift: self
]

{ #category : 'types' }
DRUnsignedRightShift >> typeWithAlreadySeen: _ [

	^ DRUnsignedIntegerType size: 8 "bytes per word"
]
