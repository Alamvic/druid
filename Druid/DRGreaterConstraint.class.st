Class {
	#name : #DRGreaterConstraint,
	#superclass : #DRSimpleConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #arithmetic }
DRGreaterConstraint >> includesValue: otherConstantValue [

	^ otherConstantValue > constantValue
]

{ #category : #arithmetic }
DRGreaterConstraint >> includesValuesGreaterThan: otherConstantValue [

	^ otherConstantValue >= constantValue 
]

{ #category : #arithmetic }
DRGreaterConstraint >> includesValuesLessThan: aConstantValue [

	^ false
]

{ #category : #arithmetic }
DRGreaterConstraint >> isIncludedIn: otherConstraint [

	^ otherConstraint includesValuesGreaterThan: constantValue
]

{ #category : #arithmetic }
DRGreaterConstraint >> negated [

	^ DRLessOrEqualsConstraint withValue: constantValue
]

{ #category : #printing }
DRGreaterConstraint >> opCode [

	^ '>'
]
