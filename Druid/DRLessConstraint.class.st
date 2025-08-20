Class {
	#name : 'DRLessConstraint',
	#superclass : 'DRRangedConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'converting' }
DRLessConstraint >> asZ3ConstraintOnVar: varName forSolver: aZ3ConstraintSolver [

	^ varName toInt < (constantValue asDRValue isNumberValue
		   ifTrue: [ constantValue ]
		   ifFalse: [ constantValue name ])
]

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
