Class {
	#name : 'DRPiGreaterThanConstraint',
	#superclass : 'DRPiNodeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'arithmetic' }
DRPiGreaterThanConstraint >> asZ3ConstraintOnVar: aDRValue withSolver: aZ3Solver [

	^ self binaryConstraint: #> on: aDRValue withSolver: aZ3Solver
]

{ #category : 'accessing' }
DRPiGreaterThanConstraint >> constantConstraintClass [

	^ DRGreaterConstraint
]

{ #category : 'arithmetic' }
DRPiGreaterThanConstraint >> negated [

	^ DRPiLessOrEqualsConstraint withValue: otherValue
]

{ #category : 'arithmetic' }
DRPiGreaterThanConstraint >> reversed [

	^ DRPiLessThanConstraint withValue: otherValue
]
