Class {
	#name : 'DRPiLessOrEqualsConstraint',
	#superclass : 'DRPiNodeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'arithmetic' }
DRPiLessOrEqualsConstraint >> asZ3ConstraintOnVar: aZ3Variable [

	^ self binaryConstraint: #'<=' on: aZ3Variable
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
