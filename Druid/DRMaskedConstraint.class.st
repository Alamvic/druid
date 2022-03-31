Class {
	#name : #DRMaskedConstraint,
	#superclass : #DRSimpleConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #arithmetic }
DRMaskedConstraint >> hasAllValuesGreaterThan: otherConstantValue [

	^ false
]

{ #category : #arithmetic }
DRMaskedConstraint >> hasAllValuesLessThan: otherConstantValue [

	^ false
]
