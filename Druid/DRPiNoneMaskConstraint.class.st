Class {
	#name : 'DRPiNoneMaskConstraint',
	#superclass : 'DRPiNodeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'arithmetic' }
DRPiNoneMaskConstraint >> asZ3ConstraintOnVar: aZ3Variable [

	^ (aZ3Variable bitAnd: (self ifConstantDo: #yourself ifNot: #name))
	  === 0
]

{ #category : 'accessing' }
DRPiNoneMaskConstraint >> constantConstraintClass [

	^ DRNoneMaskConstraint
]

{ #category : 'arithmetic' }
DRPiNoneMaskConstraint >> negated [

	^ DRPiAnyMaskConstraint withValue: otherValue
]

{ #category : 'arithmetic' }
DRPiNoneMaskConstraint >> reversed [

	^ self
]
