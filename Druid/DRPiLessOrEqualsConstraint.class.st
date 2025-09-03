Class {
	#name : 'DRPiLessOrEqualsConstraint',
	#superclass : 'DRPiNodeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'arithmetic' }
DRPiLessOrEqualsConstraint >> asZ3ConstraintOnVar: aDRValue withSolver: aZ3Solver [

	^ self binaryConstraint: #'<=' on: aDRValue withSolver: aZ3Solver
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
