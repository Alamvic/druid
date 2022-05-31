Class {
	#name : #DRMaskedConstraint,
	#superclass : #DRSimpleConstraint,
	#category : #'Druid-IR-Paths'
}

{ #category : #arithmetic }
DRMaskedConstraint >> hasAllValuesGreaterThan: otherConstantValue [

	^ false
]

{ #category : #arithmetic }
DRMaskedConstraint >> hasAllValuesLessThan: otherConstantValue [

	^ false
]
