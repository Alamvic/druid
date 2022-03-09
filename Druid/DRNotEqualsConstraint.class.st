Class {
	#name : #DRNotEqualsConstraint,
	#superclass : #DRConditionalConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #arithmetic }
DRNotEqualsConstraint >> includesValue: otherConstantValue [

	^ otherConstantValue ~= constantValue 
]

{ #category : #arithmetic }
DRNotEqualsConstraint >> includesValuesGreaterThan: otherConstantValue [

	^ otherConstantValue >= constantValue
]

{ #category : #arithmetic }
DRNotEqualsConstraint >> includesValuesLessThan: otherConstantValue [

	^ otherConstantValue <= constantValue
]

{ #category : #arithmetic }
DRNotEqualsConstraint >> isIncludedIn: otherConstraint [

	^ (otherConstraint includesValuesLessThan: constantValue) and: [ otherConstraint includesValuesGreaterThan: constantValue ]
]

{ #category : #arithmetic }
DRNotEqualsConstraint >> negated [

	^ DREqualsConstraint withValue: constantValue
]

{ #category : #printing }
DRNotEqualsConstraint >> opCode [

	^ '~='
]
