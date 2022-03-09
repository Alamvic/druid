Class {
	#name : #DRGreaterOrEqualsConstraint,
	#superclass : #DRConditionalConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #arithmetic }
DRGreaterOrEqualsConstraint >> includesValue: otherConstantValue [

	^ otherConstantValue >= constantValue
]

{ #category : #arithmetic }
DRGreaterOrEqualsConstraint >> includesValuesGreaterThan: otherConstantValue [

	^ otherConstantValue >= constantValue 
]

{ #category : #arithmetic }
DRGreaterOrEqualsConstraint >> includesValuesLessThan: aConstantValue [

	^ false
]

{ #category : #arithmetic }
DRGreaterOrEqualsConstraint >> isIncludedIn: otherConstraint [

	^ (otherConstraint includesValue: constantValue) and: [ otherConstraint includesValuesGreaterThan: constantValue ]
]

{ #category : #arithmetic }
DRGreaterOrEqualsConstraint >> negated [

	^ DRLessConstraint withValue: constantValue
]
