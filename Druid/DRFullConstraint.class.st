Class {
	#name : #DRFullConstraint,
	#superclass : #DRConditionalConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #testing }
DRFullConstraint >> hasAllValuesWithMask: aMask [

	^ true
]

{ #category : #testing }
DRFullConstraint >> hasAllValuesWithoutMask: aMask [

	^ true
]

{ #category : #testing }
DRFullConstraint >> includes: otherConstraint [

	^ true
]

{ #category : #arithmetic }
DRFullConstraint >> includesValue: otherConstantValue [

	^ true
]

{ #category : #arithmetic }
DRFullConstraint >> includesValuesGreaterThan: otherConstantValue [

	^ true
]

{ #category : #arithmetic }
DRFullConstraint >> includesValuesLessThan: otherConstantValue [

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
