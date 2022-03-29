Class {
	#name : #DRNotEqualsConstraint,
	#superclass : #DRSimpleConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #testing }
DRNotEqualsConstraint >> hasAllValuesWithMask: aMask [

	^ (constantValue anyMask: aMask) not
]

{ #category : #testing }
DRNotEqualsConstraint >> hasAllValuesWithoutMask: aMask [
	
	^ constantValue anyMask: aMask
]

{ #category : #testing }
DRNotEqualsConstraint >> hasAnyValueWithMask: aMask [
	^ true "always?"
]

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
