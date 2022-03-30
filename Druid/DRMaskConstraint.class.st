Class {
	#name : #DRMaskConstraint,
	#superclass : #DRMaskedConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #testing }
DRMaskConstraint >> hasAllValuesWithMask: aMask [

	^ constantValue allMask: aMask
]

{ #category : #testing }
DRMaskConstraint >> hasAllValuesWithoutMask: aMask [

	^ false "always?"
]

{ #category : #arithmetic }
DRMaskConstraint >> includesValue: otherConstantValue [

	^ otherConstantValue anyMask: constantValue 
]

{ #category : #arithmetic }
DRMaskConstraint >> isIncludedIn: otherConstraint [

	^ otherConstraint hasAllValuesWithMask: constantValue
]

{ #category : #arithmetic }
DRMaskConstraint >> negated [

	^ DRNotMaskConstraint withValue: constantValue
]

{ #category : #printing }
DRMaskConstraint >> opCode [

	^ 'Mask'
]
