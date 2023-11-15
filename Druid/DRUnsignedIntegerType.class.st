Class {
	#name : #DRUnsignedIntegerType,
	#superclass : #DRTopType,
	#category : #'Druid-IR-Types'
}

{ #category : #interpreting }
DRUnsignedIntegerType >> bitShift: shiftValue [

	^ shiftValue > 0
		ifTrue: [ self << shiftValue ]
		ifFalse: [ self >> shiftValue negated ]
]

{ #category : #accessing }
DRUnsignedIntegerType >> systemClass [
	
	^ Integer
]
