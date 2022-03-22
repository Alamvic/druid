Class {
	#name : #DRMaskConstraint,
	#superclass : #DRSimpleConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #arithmetic }
DRMaskConstraint >> includesValue: otherConstantValue [

	^ otherConstantValue anyMask: constantValue 
]

{ #category : #arithmetic }
DRMaskConstraint >> isIncludedIn: otherConstraint [

	^ otherConstraint hasAllValuesWithMask: constantValue
]

{ #category : #arithmetic }
DRMaskConstraint >> negated [

	^ DRNotMaskConstraint withValue: constantValue
]

{ #category : #printing }
DRMaskConstraint >> opCode [

	^ 'Mask'
]
