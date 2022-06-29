Class {
	#name : #DRGreaterConstraint,
	#superclass : #DRRangedConstraint,
	#category : #'Druid-IR-Paths'
}

{ #category : #arithmetic }
DRGreaterConstraint >> hasAllValuesGreaterThan: otherConstantValue [

	^ otherConstantValue >= constantValue 
]

{ #category : #arithmetic }
DRGreaterConstraint >> hasAllValuesLessThan: aConstantValue [

	^ false
]

{ #category : #arithmetic }
DRGreaterConstraint >> hasValue: otherConstantValue [

	^ otherConstantValue > constantValue
]

{ #category : #arithmetic }
DRGreaterConstraint >> isIncludedIn: otherConstraint [

	^ otherConstraint hasAllValuesGreaterThan: constantValue
]

{ #category : #arithmetic }
DRGreaterConstraint >> negated [

	^ DRLessOrEqualsConstraint withValue: constantValue
]

{ #category : #printing }
DRGreaterConstraint >> opCode [

	^ '>'
]

{ #category : #reversing }
DRGreaterConstraint >> reversed [
	
	^ DRLessConstraint new
]
