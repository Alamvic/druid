Class {
	#name : #DREqualsConstraint,
	#superclass : #DRSimpleConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #arithmetic }
DREqualsConstraint >> hasAllValuesGreaterThan: aConstantValue [

	^ false
]

{ #category : #arithmetic }
DREqualsConstraint >> hasAllValuesLessThan: aConstantValue [

	^ false
]

{ #category : #testing }
DREqualsConstraint >> hasAllValuesWithMask: aMask [

	^ false "always?"
]

{ #category : #testing }
DREqualsConstraint >> hasAllValuesWithoutMask: aMask [

	^ false "always?"
]

{ #category : #arithmetic }
DREqualsConstraint >> hasValue: otherConstantValue [

	^ constantValue = otherConstantValue
]

{ #category : #arithmetic }
DREqualsConstraint >> isIncludedIn: otherConstraint [

	^ otherConstraint hasValue: constantValue.
]

{ #category : #arithmetic }
DREqualsConstraint >> negated [

	^ DRNotEqualsConstraint withValue: constantValue
]

{ #category : #printing }
DREqualsConstraint >> opCode [

	^ '='
]
