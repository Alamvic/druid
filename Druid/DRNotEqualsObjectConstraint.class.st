Class {
	#name : #DRNotEqualsObjectConstraint,
	#superclass : #DRNotEqualsConstraint,
	#category : #'Druid-IR-Paths'
}

{ #category : #testing }
DRNotEqualsObjectConstraint >> hasAllValuesGreaterThan: otherConstantValue [

	^ false
]

{ #category : #testing }
DRNotEqualsObjectConstraint >> hasAllValuesLessThan: otherConstantValue [

	^ false
]

{ #category : #testing }
DRNotEqualsObjectConstraint >> hasAllValuesWithMask: otherConstantValue [

	^ false
]

{ #category : #testing }
DRNotEqualsObjectConstraint >> hasAllValuesWithoutMask: otherConstantValue [

	^ false
]

{ #category : #testing }
DRNotEqualsObjectConstraint >> hasObjectValue: objectValue [

	^ constantValue ~= objectValue
]

{ #category : #testing }
DRNotEqualsObjectConstraint >> isIncludedIn: otherConstraint [

	^ self = otherConstraint
]

{ #category : #arithmetic }
DRNotEqualsObjectConstraint >> negated [

	^ DREqualsObjectConstraint withValue: constantValue
]
