Class {
	#name : #DRFullConstraint,
	#superclass : #DRConditionalConstraint,
	#category : #'Druid-IR-Paths'
}

{ #category : #arithmetic }
DRFullConstraint >> hasAllValuesGreaterThan: otherConstantValue [

	^ true
]

{ #category : #arithmetic }
DRFullConstraint >> hasAllValuesLessThan: otherConstantValue [

	^ true
]

{ #category : #testing }
DRFullConstraint >> hasAllValuesWithMask: aMask [

	^ true
]

{ #category : #testing }
DRFullConstraint >> hasAllValuesWithoutMask: aMask [

	^ true
]

{ #category : #arithmetic }
DRFullConstraint >> hasValue: otherConstantValue [

	^ true
]

{ #category : #testing }
DRFullConstraint >> includes: otherConstraint [

	^ true
]

{ #category : #arithmetic }
DRFullConstraint >> negated [

	^ DREmptyConstraint new
]

{ #category : #printing }
DRFullConstraint >> opCode [

	^ 'ANY'
]
