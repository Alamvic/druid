Class {
	#name : #DREmptyConstraint,
	#superclass : #DRConditionalConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #testing }
DREmptyConstraint >> hasAllValuesWithMask: aMask [

	^ false
]

{ #category : #testing }
DREmptyConstraint >> hasAllValuesWithoutMask: aMask [

	^ false
]

{ #category : #testing }
DREmptyConstraint >> includes: otherConstraint [

	^ otherConstraint isDead
]

{ #category : #arithmetic }
DREmptyConstraint >> includesValue: otherConstantValue [

	^ false
]

{ #category : #arithmetic }
DREmptyConstraint >> includesValuesGreaterThan: otherConstantValue [

	^ false
]

{ #category : #arithmetic }
DREmptyConstraint >> includesValuesLessThan: otherConstantValue [

	^ false
]

{ #category : #testing }
DREmptyConstraint >> isDead [

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
