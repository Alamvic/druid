Class {
	#name : 'DRPiAnyMaskConstraint',
	#superclass : 'DRPiNodeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'arithmetic' }
DRPiAnyMaskConstraint >> asZ3ConstraintOnVar: aZ3Variable [

	^ (aZ3Variable bitAnd:
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
