Class {
	#name : 'DRPiAnyMaskConstraint',
	#superclass : 'DRPiNodeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

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
