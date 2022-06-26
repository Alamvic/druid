Class {
	#name : #DRGreaterOrEqualsConstraint,
	#superclass : #DRRangedConstraint,
	#category : #'Druid-IR-Paths'
}

{ #category : #arithmetic }
DRGreaterOrEqualsConstraint >> hasAllValuesGreaterThan: otherConstantValue [

	^ otherConstantValue >= constantValue 
]

{ #category : #arithmetic }
DRGreaterOrEqualsConstraint >> hasAllValuesLessThan: aConstantValue [

	^ false
]

{ #category : #arithmetic }
DRGreaterOrEqualsConstraint >> hasValue: otherConstantValue [

	^ otherConstantValue >= constantValue
]

{ #category : #arithmetic }
DRGreaterOrEqualsConstraint >> isIncludedIn: otherConstraint [

	^ (otherConstraint hasValue: constantValue) and: [ otherConstraint hasAllValuesGreaterThan: constantValue ]
]

{ #category : #arithmetic }
DRGreaterOrEqualsConstraint >> negated [

	^ DRLessConstraint withValue: constantValue
]

{ #category : #printing }
DRGreaterOrEqualsConstraint >> opCode [

	^ '>='
]

{ #category : #reversing }
DRGreaterOrEqualsConstraint >> reversed [
	
	^ DRLessOrEqualsConstraint withValue: constantValue
]
