Class {
	#name : 'DRPiNotEqualsConstraint',
	#superclass : 'DRPiNodeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'arithmetic' }
DRPiNotEqualsConstraint >> asZ3ConstraintOnVar: aZ3Variable [

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
