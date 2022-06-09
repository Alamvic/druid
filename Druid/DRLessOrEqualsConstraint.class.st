Class {
	#name : #DRLessOrEqualsConstraint,
	#superclass : #DRRangedConstraint,
	#category : #'Druid-IR-Paths'
}

{ #category : #arithmetic }
DRLessOrEqualsConstraint >> hasAllValuesGreaterThan: otherConstantValue [

	^ false
]

{ #category : #arithmetic }
DRLessOrEqualsConstraint >> hasAllValuesLessThan: otherConstantValue [

	^ otherConstantValue <= constantValue
]

{ #category : #arithmetic }
DRLessOrEqualsConstraint >> hasValue: otherConstantValue [

	^ otherConstantValue <= constantValue
]

{ #category : #arithmetic }
DRLessOrEqualsConstraint >> isIncludedIn: otherConstraint [

	^ (otherConstraint hasValue: constantValue) and: [otherConstraint hasAllValuesLessThan: constantValue]
]

{ #category : #arithmetic }
DRLessOrEqualsConstraint >> negated [

	^ DRGreaterConstraint withValue: constantValue
]

{ #category : #printing }
DRLessOrEqualsConstraint >> opCode [

	^ '<='
]
