Class {
	#name : 'DRPiAnyMaskConstraint',
	#superclass : 'DRPiNodeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'arithmetic' }
DRPiAnyMaskConstraint >> asZ3ConstraintOnVar: aDRPiNode withSolver: aZ3Solver [

	^ ((aZ3Solver drValAsZ3Val: aDRPiNode) bitAnd:
		  (self ifConstantDo: [ :val | val ] ifNot: [ :val | val name ]))
		  ~== 0
]

{ #category : 'accessing' }
DRPiAnyMaskConstraint >> constantConstraintClass [

	^ DRAnyMaskConstraint
]

{ #category : 'arithmetic' }
DRPiAnyMaskConstraint >> negated [

	^ DRPiNoneMaskConstraint withValue: otherValue
]

{ #category : 'arithmetic' }
DRPiAnyMaskConstraint >> reversed [

	^ self
]
