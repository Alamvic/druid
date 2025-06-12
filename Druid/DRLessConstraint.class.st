Class {
	#name : 'DRLessConstraint',
	#superclass : 'DRRangedConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'reversing' }
DRLessConstraint >> distanceForBuildingABCDGraph [

	^ -1
]

{ #category : 'arithmetic' }
DRLessConstraint >> hasAllValuesGreaterThan: otherConstantValue [

	^ false
]

{ #category : 'arithmetic' }
DRLessConstraint >> hasAllValuesLessThan: otherConstantValue [

	^ otherConstantValue <= constantValue
]

{ #category : 'arithmetic' }
DRLessConstraint >> hasValue: otherConstantValue [

	^ otherConstantValue < constantValue
]

{ #category : 'arithmetic' }
DRLessConstraint >> isIncludedIn: otherConstraint [

	^ otherConstraint hasAllValuesLessThan: constantValue
]

{ #category : 'arithmetic' }
DRLessConstraint >> negated [

	^ DRGreaterOrEqualsConstraint withValue: constantValue
]

{ #category : 'printing' }
DRLessConstraint >> opCode [

	^ '<'
]

{ #category : 'reversing' }
DRLessConstraint >> reversed [
	
	^ DRGreaterConstraint withValue: constantValue
]
