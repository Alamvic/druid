Class {
	#name : 'DRPiEqualsConstraint',
	#superclass : 'DRPiNodeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'arithmetic' }
DRPiEqualsConstraint >> asZ3ConstraintOnVar: varName [

	^ self binaryConstraint: #=== on: varName
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
