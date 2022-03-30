Class {
	#name : #DRNotMaskConstraint,
	#superclass : #DRMaskedConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #testing }
DRNotMaskConstraint >> hasAllValuesWithMask: aMask [

	^ false "always?"
]

{ #category : #testing }
DRNotMaskConstraint >> hasAllValuesWithoutMask: aMask [ 
	
	^ aMask allMask: constantValue
]

{ #category : #arithmetic }
DRNotMaskConstraint >> includesValue: otherConstantValue [

	^ (otherConstantValue anyMask: constantValue) not
]

{ #category : #arithmetic }
DRNotMaskConstraint >> isIncludedIn: otherConstraint [

	^ otherConstraint hasAllValuesWithoutMask: constantValue
]

{ #category : #arithmetic }
DRNotMaskConstraint >> negated [

	^ DRMaskConstraint withValue: constantValue
]

{ #category : #printing }
DRNotMaskConstraint >> opCode [

	^ 'Not mask'
]
