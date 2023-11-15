Class {
	#name : #DRSignedIntegerType,
	#superclass : #DRTopType,
	#category : #'Druid-IR-Types'
}

{ #category : #interpreting }
DRSignedIntegerType >> bitShift: shiftValue [

	^ shiftValue > 0
		ifTrue: [ self << shiftValue ]
		ifFalse: [ self >> shiftValue negated ]
]

{ #category : #accessing }
DRSignedIntegerType >> systemClass [
	
	^ Integer
]
