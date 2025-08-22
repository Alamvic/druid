Class {
	#name : 'DRPiNoneMaskConstraint',
	#superclass : 'DRPiNodeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

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
