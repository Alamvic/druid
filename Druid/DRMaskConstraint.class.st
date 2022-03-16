Class {
	#name : #DRMaskConstraint,
	#superclass : #DRSimpleConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #arithmetic }
DRMaskConstraint >> negated [

	^ DRNotMaskConstraint withValue: constantValue
]

{ #category : #printing }
DRMaskConstraint >> opCode [

	^ 'Mask'
]
