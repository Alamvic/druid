Class {
	#name : 'DRPiLessThanConstraint',
	#superclass : 'DRPiNodeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'arithmetic' }
DRPiLessThanConstraint >> asZ3ConstraintOnVar: aDRValue withSolver: aZ3Solver [

	^ self binaryConstraint: #< on: aDRValue withSolver: aZ3Solver
]

{ #category : 'adding' }
DRPiLessThanConstraint >> constantConstraintClass [

	^ DRLessConstraint
]

{ #category : 'arithmetic' }
DRPiLessThanConstraint >> distanceForBuildingABCDGraph [

	^ -1
]

{ #category : 'arithmetic' }
DRPiLessThanConstraint >> negated [

	^ DRPiGreaterOrEqualsConstraint withValue: otherValue
]

{ #category : 'arithmetic' }
DRPiLessThanConstraint >> reversed [

	^ DRPiGreaterThanConstraint withValue: otherValue
]
