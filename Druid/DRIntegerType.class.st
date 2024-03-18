Class {
	#name : #DRIntegerType,
	#superclass : #DRTopType,
	#instVars : [
		'size'
	],
	#category : #'Druid-IR-Types'
}

{ #category : #factory }
DRIntegerType class >> bits64 [

	^ self size: 8 "bytes per word"
]

{ #category : #accessing }
DRIntegerType class >> size: aSize [

	^ self new
		  size: aSize;
		  yourself
]

{ #category : #testing }
DRIntegerType >> isIntegerType [
	
	^ true
]

{ #category : #'accessing - query' }
DRIntegerType >> numberOfBits [

	^ self size * 8
]

{ #category : #printing }
DRIntegerType >> printOn: aStream [

	super printOn: aStream.
	aStream << '[' << self size asString << ']'
]

{ #category : #accessing }
DRIntegerType >> size [

	^ size
]

{ #category : #accessing }
DRIntegerType >> size: anObject [

	size := anObject
]
