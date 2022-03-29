Class {
	#name : #DREqualsConstraint,
	#superclass : #DRSimpleConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #testing }
DREqualsConstraint >> hasAllValuesWithMask: aMask [

	^ false "always?"
]

{ #category : #testing }
DREqualsConstraint >> hasAllValuesWithoutMask: aMask [

	^ false "always?"
]

{ #category : #arithmetic }
DREqualsConstraint >> includesValue: otherConstantValue [

	^ constantValue = otherConstantValue
]

{ #category : #arithmetic }
DREqualsConstraint >> includesValuesGreaterThan: aConstantValue [

	^ false
]

{ #category : #arithmetic }
DREqualsConstraint >> includesValuesLessThan: aConstantValue [

	^ false
]

{ #category : #arithmetic }
DREqualsConstraint >> isIncludedIn: otherConstraint [

	^ otherConstraint includesValue: constantValue.
]

{ #category : #arithmetic }
DREqualsConstraint >> negated [

	^ DRNotEqualsConstraint withValue: constantValue
]

{ #category : #printing }
DREqualsConstraint >> opCode [

	^ '='
]
