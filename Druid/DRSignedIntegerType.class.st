Class {
	#name : #DRSignedIntegerType,
	#superclass : #DRIntegerType,
	#category : #'Druid-IR-Types'
}

{ #category : #interpreting }
DRSignedIntegerType >> bitShift: shiftValue [

	^ shiftValue > 0
		ifTrue: [ self << shiftValue ]
		ifFalse: [ self >> shiftValue negated ]
]

{ #category : #testing }
DRSignedIntegerType >> isUnsigned [
	
	^ false
]

{ #category : #accessing }
DRSignedIntegerType >> systemClass [
	
	^ Integer
]
