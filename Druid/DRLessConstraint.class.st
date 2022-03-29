Class {
	#name : #DRLessConstraint,
	#superclass : #DRRangeConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #arithmetic }
DRLessConstraint >> includesValue: otherConstantValue [

	^ otherConstantValue < constantValue
]

{ #category : #arithmetic }
DRLessConstraint >> includesValuesGreaterThan: otherConstantValue [

	^ false
]

{ #category : #arithmetic }
DRLessConstraint >> includesValuesLessThan: otherConstantValue [

	^ otherConstantValue <= constantValue
]

{ #category : #arithmetic }
DRLessConstraint >> isIncludedIn: otherConstraint [

	^ otherConstraint includesValuesLessThan: constantValue
]

{ #category : #arithmetic }
DRLessConstraint >> negated [

	^ DRGreaterOrEqualsConstraint withValue: constantValue
]

{ #category : #printing }
DRLessConstraint >> opCode [

	^ '<'
]
