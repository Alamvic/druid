Class {
	#name : 'DRPiEqualsConstraint',
	#superclass : 'DRPiNodeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'arithmetic' }
DRPiEqualsConstraint >> asZ3ConstraintOnVar: aZ3Variable [

	| inner |
	inner := self z3VarName.
	(inner isBooleanValue and: [ aZ3Variable isBitVector ]) ifTrue: [
		^ aZ3Variable ~== 0 === inner ].
	^ self binaryConstraint: #'===' on: aZ3Variable
]

{ #category : 'accessing' }
DRPiEqualsConstraint >> constantConstraintClass [

	^ DREqualsConstraint
]

{ #category : 'arithmetic' }
DRPiEqualsConstraint >> distanceForBuildingABCDGraph [

	^ 0
]

{ #category : 'arithmetic' }
DRPiEqualsConstraint >> negated [

	^ DRPiNotEqualsConstraint withValue: otherValue
]

{ #category : 'initialization' }
DRPiEqualsConstraint >> reversed [

	^ self
]
