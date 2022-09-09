Class {
	#name : #DRUndefinedType,
	#superclass : #DRTopType,
	#category : #'Druid-IR-Types'
}

{ #category : #interpreting }
DRUndefinedType >> bitShift: shiftValue [

	^ shiftValue > 0
		ifTrue: [ self << shiftValue ]
		ifFalse: [ self >> shiftValue negated ]
]
