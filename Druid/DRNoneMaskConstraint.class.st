Class {
	#name : 'DRNoneMaskConstraint',
	#superclass : 'DRMaskedConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'testing' }
DRNoneMaskConstraint >> hasAllValuesWithMask: aMask [

	^ false "always?"
]

{ #category : 'testing' }
DRNoneMaskConstraint >> hasAllValuesWithoutMask: aMask [ 
	
	^ aMask allMask: constantValue
]

{ #category : 'arithmetic' }
DRNoneMaskConstraint >> hasValue: otherConstantValue [

	^ (otherConstantValue anyMask: constantValue) not
]

{ #category : 'arithmetic' }
DRNoneMaskConstraint >> isIncludedIn: otherConstraint [

	^ otherConstraint hasAllValuesWithoutMask: constantValue
]

{ #category : 'arithmetic' }
DRNoneMaskConstraint >> negated [

	^ DRAnyMaskConstraint withValue: constantValue
]

{ #category : 'printing' }
DRNoneMaskConstraint >> opCode [

	^ 'Not mask'
]
