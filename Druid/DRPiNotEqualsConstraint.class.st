Class {
	#name : 'DRPiNotEqualsConstraint',
	#superclass : 'DRPiNodeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'arithmetic' }
DRPiNotEqualsConstraint >> asZ3ConstraintOnVar: varName [

	^ self binaryConstraint: #~== on: varName
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
