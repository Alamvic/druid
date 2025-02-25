Class {
	#name : 'DREqualsConstraint',
	#superclass : 'DRSimpleConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'initialization' }
DREqualsConstraint class >> withValue: aConstantValue [

	| value |
	value := aConstantValue asDRValue.

	(value isObjectReferenceValue or: [ value isNumberValue not]) ifTrue: [ 
		^ DREqualsObjectConstraint new
			  constantValue: value;
			  yourself ].

	^ super withValue: value
]

{ #category : 'arithmetic' }
DREqualsConstraint >> hasAllValuesGreaterThan: aConstantValue [

	^ false
]

{ #category : 'arithmetic' }
DREqualsConstraint >> hasAllValuesLessThan: aConstantValue [

	^ false
]

{ #category : 'testing' }
DREqualsConstraint >> hasAllValuesWithMask: aMask [

	^ false "always?"
]

{ #category : 'testing' }
DREqualsConstraint >> hasAllValuesWithoutMask: aMask [

	^ false "always?"
]

{ #category : 'arithmetic' }
DREqualsConstraint >> hasValue: otherConstantValue [

	^ constantValue = otherConstantValue
]

{ #category : 'arithmetic' }
DREqualsConstraint >> isIncludedIn: otherConstraint [

	^ otherConstraint hasValue: constantValue.
]

{ #category : 'arithmetic' }
DREqualsConstraint >> negated [

	^ DRNotEqualsConstraint new
		  constantValue: constantValue;
		  yourself
]

{ #category : 'printing' }
DREqualsConstraint >> opCode [

	^ '='
]

{ #category : 'reversing' }
DREqualsConstraint >> reversed [
	
	^ self
]
