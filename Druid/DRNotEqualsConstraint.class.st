Class {
	#name : #DRNotEqualsConstraint,
	#superclass : #DRSimpleConstraint,
	#category : #'Druid-IR-Paths'
}

{ #category : #initialization }
DRNotEqualsConstraint class >> withValue: aConstantValue [

	| value |
	value := aConstantValue asDRValue.

	(value isObjectReferenceValue or: [ value isNumberValue not]) ifTrue: [ 
		^ DRNotEqualsObjectConstraint new
			  constantValue: value;
			  yourself ].

	^ super withValue: value
]

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

{ #category : #testing }
DRNotEqualsConstraint >> hasObjectValue: objectValue [

	^ true
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

	^ DREqualsConstraint new
		  constantValue: constantValue;
		  yourself
]

{ #category : #printing }
DRNotEqualsConstraint >> opCode [

	^ '~='
]

{ #category : #reversing }
DRNotEqualsConstraint >> reversed [
	
	^ self
]
