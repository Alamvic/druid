Class {
	#name : #DRIntegerType,
	#superclass : #DRTopType,
	#category : #'Druid-IR-Types'
}

{ #category : #interpreting }
DRIntegerType >> bitShift: shiftValue [

	^ shiftValue > 0
		ifTrue: [ self << shiftValue ]
		ifFalse: [ self >> shiftValue negated ]
]

{ #category : #accessing }
DRIntegerType >> systemClass [
	
	^ Integer
]
