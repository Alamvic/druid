Class {
	#name : 'DRLessOrEqualsConstraint',
	#superclass : 'DRRangedConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'testing' }
DRLessOrEqualsConstraint >> asZ3ConstraintOnVar: varName forSolver: aZ3ConstraintSolver [

	^ varName toInt <= (constantValue asDRValue isNumberValue
		   ifTrue: [ constantValue ]
		   ifFalse: [ constantValue name ])
]

{ #category : 'reversing' }
DRLessOrEqualsConstraint >> distanceForBuildingABCDGraph [

	^ 0
]

{ #category : 'arithmetic' }
DRLessOrEqualsConstraint >> hasAllValuesGreaterThan: otherConstantValue [

	^ false
]

{ #category : 'arithmetic' }
DRLessOrEqualsConstraint >> hasAllValuesLessThan: otherConstantValue [

	^ otherConstantValue <= constantValue
]

{ #category : 'arithmetic' }
DRLessOrEqualsConstraint >> hasValue: otherConstantValue [

	^ otherConstantValue <= constantValue
]

{ #category : 'arithmetic' }
DRLessOrEqualsConstraint >> isIncludedIn: otherConstraint [

	^ (otherConstraint hasValue: constantValue) and: [otherConstraint hasAllValuesLessThan: constantValue]
]

{ #category : 'arithmetic' }
DRLessOrEqualsConstraint >> negated [

	^ DRGreaterConstraint withValue: constantValue
]

{ #category : 'printing' }
DRLessOrEqualsConstraint >> opCode [

	^ '<='
]

{ #category : 'reversing' }
DRLessOrEqualsConstraint >> reversed [

	"Returns the same condition but with reversed operands: x <= y --> y >= x"
	^ DRGreaterOrEqualsConstraint withValue: constantValue
]
