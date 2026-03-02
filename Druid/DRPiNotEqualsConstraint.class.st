Class {
	#name : 'DRPiNotEqualsConstraint',
	#superclass : 'DRPiNodeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'arithmetic' }
DRPiNotEqualsConstraint >> asZ3ConstraintOnVar: aZ3Variable [

	| inner |
	inner := self z3VarName.
	(inner isBooleanValue and: [ aZ3Variable isBitVector ]) ifTrue: [
		^ aZ3Variable ~== 0 ~== inner ].
	^ self binaryConstraint: #'~==' on: aZ3Variable
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
