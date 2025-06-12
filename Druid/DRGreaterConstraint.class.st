Class {
	#name : 'DRGreaterConstraint',
	#superclass : 'DRRangedConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'arithmetic' }
DRGreaterConstraint >> hasAllValuesGreaterThan: otherConstantValue [

	^ otherConstantValue >= constantValue 
]

{ #category : 'arithmetic' }
DRGreaterConstraint >> hasAllValuesLessThan: aConstantValue [

	^ false
]

{ #category : 'arithmetic' }
DRGreaterConstraint >> hasValue: otherConstantValue [

	^ otherConstantValue > constantValue
]

{ #category : 'arithmetic' }
DRGreaterConstraint >> isIncludedIn: otherConstraint [

	^ otherConstraint hasAllValuesGreaterThan: constantValue
]

{ #category : 'reversing' }
DRGreaterConstraint >> minABCDDistanceToBeSatisfiable [

	"TODO
	I'm not sure, but I think returning 1 here is fine, and may allow it to optimize more cases.
	Needs testing though"
	"^ 1"
	^ nil
]

{ #category : 'arithmetic' }
DRGreaterConstraint >> negated [

	^ DRLessOrEqualsConstraint withValue: constantValue
]

{ #category : 'printing' }
DRGreaterConstraint >> opCode [

	^ '>'
]

{ #category : 'reversing' }
DRGreaterConstraint >> reversed [
	
	^ DRLessConstraint withValue: constantValue
]
