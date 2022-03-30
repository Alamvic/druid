Class {
	#name : #DRMaskedConstraint,
	#superclass : #DRSimpleConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #arithmetic }
DRMaskedConstraint >> includesValuesGreaterThan: otherConstantValue [

	^ false
]

{ #category : #arithmetic }
DRMaskedConstraint >> includesValuesLessThan: otherConstantValue [

	^ false
]
