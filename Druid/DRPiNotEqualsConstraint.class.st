Class {
	#name : 'DRPiNotEqualsConstraint',
	#superclass : 'DRPiNodeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'arithmetic' }
DRPiNotEqualsConstraint >> asZ3ConstraintOnVar: aDRValue withSolver: aZ3Solver [

	^ self binaryConstraint: #'~==' on: aDRValue withSolver: aZ3Solver
]

{ #category : 'accessing' }
DRPiNotEqualsConstraint >> constantConstraintClass [

	^ DRNotEqualsConstraint
]

{ #category : 'arithmetic' }
DRPiNotEqualsConstraint >> negated [

	^ DRPiEqualsConstraint withValue: otherValue
]

{ #category : 'arithmetic' }
DRPiNotEqualsConstraint >> reversed [

	^ self
]
