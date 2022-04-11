Class {
	#name : #DRNotEqualsConstraint,
	#superclass : #DRSimpleConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #arithmetic }
DRNotEqualsConstraint >> hasAllValuesGreaterThan: otherConstantValue [

	^ otherConstantValue >= constantValue
]

{ #category : #arithmetic }
DRNotEqualsConstraint >> hasAllValuesLessThan: otherConstantValue [

	^ otherConstantValue <= constantValue
]

{ #category : #testing }
DRNotEqualsConstraint >> hasAllValuesWithMask: aMask [

	^ (constantValue anyMask: aMask) not
]

{ #category : #testing }
DRNotEqualsConstraint >> hasAllValuesWithoutMask: aMask [
	
	^ constantValue anyMask: aMask
]

{ #category : #arithmetic }
DRNotEqualsConstraint >> hasValue: otherConstantValue [

	^ otherConstantValue ~= constantValue 
]

{ #category : #arithmetic }
DRNotEqualsConstraint >> isIncludedIn: otherConstraint [

	^ (otherConstraint hasAllValuesLessThan: constantValue) and: [ otherConstraint hasAllValuesGreaterThan: constantValue ]
]

{ #category : #arithmetic }
DRNotEqualsConstraint >> negated [

	^ DREqualsConstraint withValue: constantValue
]

{ #category : #printing }
DRNotEqualsConstraint >> opCode [

	^ '~='
]
