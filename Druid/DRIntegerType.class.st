Class {
	#name : #DRIntegerType,
	#superclass : #DRTopType,
	#instVars : [
		'size'
	],
	#category : #'Druid-IR-Types'
}

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
