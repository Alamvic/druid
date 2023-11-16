Class {
	#name : #DRUnsignedRightShift,
	#superclass : #DRSignedRightShift,
	#category : #'Druid-IR'
}

{ #category : #types }
DRUnsignedRightShift >> type [

	^ DRUnsignedIntegerType size: 8 "bytes per word"
]
