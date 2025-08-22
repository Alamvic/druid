Class {
	#name : 'DRPiGreaterOrEqualsConstraint',
	#superclass : 'DRPiNodeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'arithmetic' }
DRPiGreaterOrEqualsConstraint >> asZ3ConstraintOnVar: varName [

	^ self binaryConstraint: #>= on: varName
]

{ #category : 'accessing' }
DRPiGreaterOrEqualsConstraint >> constantConstraintClass [

	^ DRGreaterOrEqualsConstraint
]

{ #category : 'accessing' }
DRPiGreaterOrEqualsConstraint >> minABCDDistanceToBeSatisfiable [

	^ 0
]

{ #category : 'arithmetic' }
DRPiGreaterOrEqualsConstraint >> negated [

	^ DRPiLessThanConstraint withValue: otherValue
]

{ #category : 'arithmetic' }
DRPiGreaterOrEqualsConstraint >> reversed [

	^ DRPiLessOrEqualsConstraint withValue: otherValue
]
