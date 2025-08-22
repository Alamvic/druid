Class {
	#name : 'DRPiLessOrEqualsConstraint',
	#superclass : 'DRPiNodeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'arithmetic' }
DRPiLessOrEqualsConstraint >> asZ3ConstraintOnVar: varName [

	^ self binaryConstraint: #<= on: varName
]

{ #category : 'accessing' }
DRPiLessOrEqualsConstraint >> constantConstraintClass [

	^ DRLessOrEqualsConstraint
]

{ #category : 'arithmetic' }
DRPiLessOrEqualsConstraint >> distanceForBuildingABCDGraph [

	^ 0
]

{ #category : 'arithmetic' }
DRPiLessOrEqualsConstraint >> negated [

	^ DRPiGreaterThanConstraint withValue: otherValue
]

{ #category : 'arithmetic' }
DRPiLessOrEqualsConstraint >> reversed [

	^ DRPiGreaterOrEqualsConstraint withValue: otherValue
]
