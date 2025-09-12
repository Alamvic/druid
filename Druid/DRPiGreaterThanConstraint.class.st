Class {
	#name : 'DRPiGreaterThanConstraint',
	#superclass : 'DRPiNodeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'arithmetic' }
DRPiGreaterThanConstraint >> asZ3ConstraintOnVar: aZ3Variable [

	^ self binaryConstraint: #> on: aZ3Variable
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
