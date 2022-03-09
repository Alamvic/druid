Class {
	#name : #DRLessOrEqualsConstraint,
	#superclass : #DRConditionalConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #arithmetic }
DRLessOrEqualsConstraint >> includesValue: otherConstantValue [

	^ otherConstantValue <= constantValue
]

{ #category : #arithmetic }
DRLessOrEqualsConstraint >> includesValuesGreaterThan: otherConstantValue [

	^ false
]

{ #category : #arithmetic }
DRLessOrEqualsConstraint >> includesValuesLessThan: otherConstantValue [

	^ otherConstantValue <= constantValue
]

{ #category : #arithmetic }
DRLessOrEqualsConstraint >> isIncludedIn: otherConstraint [

	^ (otherConstraint includesValue: constantValue) and: [otherConstraint includesValuesLessThan: constantValue]
]

{ #category : #arithmetic }
DRLessOrEqualsConstraint >> negated [

	^ DRGreaterConstraint withValue: constantValue
]
