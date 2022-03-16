Class {
	#name : #DRDeadConstraint,
	#superclass : #DRConditionalConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #testing }
DRDeadConstraint >> isDead [

	^ true
]

{ #category : #printing }
DRDeadConstraint >> opCode [

	^ 'DEAD'
]
