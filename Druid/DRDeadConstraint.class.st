Class {
	#name : #DRDeadConstraint,
	#superclass : #DRConditionalConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #testing }
DRDeadConstraint >> includes: otherConstraint [

	^ otherConstraint isDead not
]

{ #category : #arithmetic }
DRDeadConstraint >> includesValue: otherConstantValue [

	^ true
]

{ #category : #arithmetic }
DRDeadConstraint >> includesValuesGreaterThan: otherConstantValue [

	^ true
]

{ #category : #arithmetic }
DRDeadConstraint >> includesValuesLessThan: otherConstantValue [

	^ true
]

{ #category : #testing }
DRDeadConstraint >> isDead [

	^ true
]

{ #category : #arithmetic }
DRDeadConstraint >> negated [

	^ self
]

{ #category : #printing }
DRDeadConstraint >> opCode [

	^ 'DEAD'
]
