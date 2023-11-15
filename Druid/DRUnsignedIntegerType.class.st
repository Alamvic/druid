Class {
	#name : #DRUnsignedIntegerType,
	#superclass : #DRIntegerType,
	#instVars : [
		'size'
	],
	#category : #'Druid-IR-Types'
}

{ #category : #accessing }
DRUnsignedIntegerType class >> size: aSize [

	^ self new
		  size: aSize;
		  yourself
]

{ #category : #interpreting }
DRUnsignedIntegerType >> bitShift: shiftValue [

	^ shiftValue > 0
		ifTrue: [ self << shiftValue ]
		ifFalse: [ self >> shiftValue negated ]
]

{ #category : #testing }
DRUnsignedIntegerType >> isSigned [
	
	^ false
]

{ #category : #testing }
DRUnsignedIntegerType >> isUnsigned [
	
	^ true
]

{ #category : #accessing }
DRUnsignedIntegerType >> size [

	^ size
]

{ #category : #accessing }
DRUnsignedIntegerType >> size: anObject [

	size := anObject
]

{ #category : #accessing }
DRUnsignedIntegerType >> systemClass [
	
	^ Integer
]
