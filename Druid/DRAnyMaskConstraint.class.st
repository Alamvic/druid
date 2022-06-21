Class {
	#name : #DRAnyMaskConstraint,
	#superclass : #DRMaskedConstraint,
	#category : #'Druid-IR-Paths'
}

{ #category : #testing }
DRAnyMaskConstraint >> hasAllValuesWithMask: aMask [

	^ constantValue allMask: aMask
]

{ #category : #testing }
DRAnyMaskConstraint >> hasAllValuesWithoutMask: aMask [

	^ false "always?"
]

{ #category : #arithmetic }
DRAnyMaskConstraint >> hasValue: otherConstantValue [

	^ otherConstantValue anyMask: constantValue 
]

{ #category : #arithmetic }
DRAnyMaskConstraint >> isIncludedIn: otherConstraint [

	^ otherConstraint hasAllValuesWithMask: constantValue
]

{ #category : #arithmetic }
DRAnyMaskConstraint >> negated [

	^ DRNoneMaskConstraint withValue: constantValue
]

{ #category : #printing }
DRAnyMaskConstraint >> opCode [

	^ 'Mask'
]
