Class {
	#name : #DREqualsRegisterConstraint,
	#superclass : #DRConditionalRegisterConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #arithmetic }
DREqualsRegisterConstraint >> includes: otherConstraint [

	^ otherConstraint includesValue: constantValue.
]

{ #category : #arithmetic }
DREqualsRegisterConstraint >> includesValue: otherConstantValue [

	^ constantValue = otherConstantValue
]
