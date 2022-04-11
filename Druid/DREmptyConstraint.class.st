Class {
	#name : #DREmptyConstraint,
	#superclass : #DRConditionalConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #arithmetic }
DREmptyConstraint >> hasAllValuesGreaterThan: otherConstantValue [

	^ false
]

{ #category : #arithmetic }
DREmptyConstraint >> hasAllValuesLessThan: otherConstantValue [

	^ false
]

{ #category : #testing }
DREmptyConstraint >> hasAllValuesWithMask: aMask [

	^ false
]

{ #category : #testing }
DREmptyConstraint >> hasAllValuesWithoutMask: aMask [

	^ false
]

{ #category : #arithmetic }
DREmptyConstraint >> hasValue: otherConstantValue [

	^ false
]

{ #category : #testing }
DREmptyConstraint >> includes: otherConstraint [

	^ otherConstraint isEmpty
]

{ #category : #testing }
DREmptyConstraint >> isEmpty [

	^ true
]

{ #category : #arithmetic }
DREmptyConstraint >> negated [

	^ DRFullConstraint new
]

{ #category : #printing }
DREmptyConstraint >> opCode [

	^ 'EMPTY'
]
