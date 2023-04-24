Class {
	#name : #DRConditionalConstraintTest,
	#superclass : #DRIRTest,
	#category : #'Druid-Tests'
}

{ #category : #constraints }
DRConditionalConstraintTest >> testEmptyConstraint [

	| constraint |
	constraint := DREmptyConstraint new.
	
	self assertConstraint: constraint includes: (DREmptyConstraint new).
	self denyConstraint: constraint includes: (DREqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRGreaterConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRLessConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRNotEqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRAnyMaskConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRNoneMaskConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRFullConstraint new).
	

	self assertConstraint: (DREqualsConstraint withValue: 10) includes: constraint.
	self assertConstraint: (DRGreaterOrEqualsConstraint withValue: 10) includes: constraint.
	self assertConstraint: (DRLessOrEqualsConstraint withValue: 10) includes: constraint.
	self assertConstraint: (DRGreaterConstraint withValue: 10) includes: constraint.
	self assertConstraint: (DRLessConstraint withValue: 10) includes: constraint.
	self assertConstraint: (DRNotEqualsConstraint withValue: 10) includes: constraint.
	self assertConstraint: (DRAnyMaskConstraint withValue: 10) includes: constraint.
	self assertConstraint: (DRNoneMaskConstraint withValue: 10) includes: constraint.
	self assertConstraint: (DRFullConstraint new) includes: constraint.
]

{ #category : #constraints }
DRConditionalConstraintTest >> testEqualsConstraint [

	| constraint |
	constraint := DREqualsConstraint withValue: 10.
	
	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DREqualsConstraint withValue: 20).
	self denyConstraint: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRGreaterConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRLessConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRNotEqualsConstraint withValue: 10).
]

{ #category : #constraints }
DRConditionalConstraintTest >> testEqualsObjectConstraint [

	| constraint |
	constraint := DREqualsObjectConstraint withValue: nil.
	
	self assertConstraint: constraint includes: constraint.
	self denyConstraint: constraint includes: (DREqualsConstraint withValue: 20).
	self denyConstraint: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRGreaterConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRLessConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRNotEqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRNotEqualsConstraint withValue: nil).
]

{ #category : #constraints }
DRConditionalConstraintTest >> testExcludeTypeConstraint [

	| constraint types |
	constraint := DRExcludeTypeConstraint withType: OrderedCollection asDRType.
	
	self deny: (constraint hasType: OrderedCollection asDRType).
	self assert: (constraint hasType: Array asDRType).

	self assertConstraint: constraint includes: (DRExcludeTypeConstraint withType: OrderedCollection asDRType).
	self assertConstraint: (DRExcludeTypeConstraint withType: OrderedCollection asDRType) includes: constraint.
	self assertConstraint: constraint includes: constraint.
	self denyConstraint: constraint includes: (DRExcludeTypeConstraint withType: Array asDRType).
	self denyConstraint: (DRExcludeTypeConstraint withType: Array asDRType) includes: constraint.
	
	self denyConstraint: constraint includes: (DRIncludeTypeConstraint withType: OrderedCollection asDRType).
	self denyConstraint: (DRIncludeTypeConstraint withType: OrderedCollection asDRType) includes: constraint.
	self assertConstraint: constraint includes: (DRIncludeTypeConstraint withType: Array asDRType).
	self denyConstraint: (DRIncludeTypeConstraint withType: Array asDRType) includes: constraint.
	
	
	types := { Array asDRType . OrderedCollection asDRType }.
	self assertConstraint: constraint includes: (DRIntersectionConstraint withoutTypes: types).
	self denyConstraint: constraint includes: (DRUnionConstraint withoutTypes: types).
	self denyConstraint: (DRIntersectionConstraint withoutTypes: types) includes: constraint.
	self assertConstraint: (DRUnionConstraint withoutTypes: types) includes: constraint.

]

{ #category : #constraints }
DRConditionalConstraintTest >> testGreaterConstraint [

	| constraint |
	constraint := DRGreaterConstraint withValue: 10.
	
	self denyConstraint: constraint includes: (DREqualsConstraint withValue: 10).
	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 11).
	self denyConstraint: constraint includes: (DREqualsConstraint withValue: 9).
	
	self denyConstraint: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	
	self assertConstraint: constraint includes: (DRGreaterConstraint withValue: 10).
	self assertConstraint: constraint includes: (DRGreaterConstraint withValue: 11).
	self denyConstraint: constraint includes: (DRGreaterConstraint withValue: 9).
	
	self denyConstraint: constraint includes: (DRLessConstraint withValue: 10).
	
	self denyConstraint: constraint includes: (DRNotEqualsConstraint withValue: 10).
	
	self assertConstraint: constraint includes: (DRIntersectionConstraint between: 10 and: 20).
	self denyConstraint: constraint includes: (DRIntersectionConstraint between: 5 and: 20).
	self denyConstraint: constraint includes: (DRIntersectionConstraint between: 5 and: 9).
	
	self denyConstraint: constraint includes: (DRUnionConstraint less: 5 greater: 15).
]

{ #category : #constraints }
DRConditionalConstraintTest >> testGreaterOrEqualsConstraint [

	| constraint |
	constraint := DRGreaterOrEqualsConstraint withValue: 10.
	
	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 10).
	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 11).
	self denyConstraint: constraint includes: (DREqualsConstraint withValue: 9).
	self assertConstraint: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	self assertConstraint: constraint includes: (DRGreaterConstraint withValue: 10).
	self assertConstraint: constraint includes: (DRGreaterConstraint withValue: 11).
	self denyConstraint: constraint includes: (DRGreaterConstraint withValue: 9).
	self denyConstraint: constraint includes: (DRLessConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRNotEqualsConstraint withValue: 10).
]

{ #category : #constraints }
DRConditionalConstraintTest >> testIncludeTypeConstraint [

	| constraint types |
	constraint := DRIncludeTypeConstraint withType: OrderedCollection asDRType.
	
	self assert: (constraint hasType: OrderedCollection asDRType).
	self deny: (constraint hasType: Array asDRType).

	self assertConstraint: constraint includes: (DRIncludeTypeConstraint withType: OrderedCollection asDRType).
	self assertConstraint: (DRIncludeTypeConstraint withType: OrderedCollection asDRType) includes: constraint.
	self assertConstraint: constraint includes: constraint.
	self denyConstraint: constraint includes: (DRIncludeTypeConstraint withType: Array asDRType).
	self denyConstraint: (DRIncludeTypeConstraint withType: Array asDRType) includes: constraint.
	
	self denyConstraint: constraint includes: (DRExcludeTypeConstraint withType: OrderedCollection asDRType).
	self denyConstraint: (DRExcludeTypeConstraint withType: OrderedCollection asDRType) includes: constraint.
	self denyConstraint: constraint includes: (DRExcludeTypeConstraint withType: Array asDRType).
	self assertConstraint: (DRExcludeTypeConstraint withType: Array asDRType) includes: constraint.
	
	
	types := { Array asDRType . OrderedCollection asDRType }.
	self assertConstraint: constraint includes: (DRIntersectionConstraint withTypes: types). "Always empty?"
	self denyConstraint: constraint includes: (DRUnionConstraint withTypes: types).
	self denyConstraint: (DRIntersectionConstraint withTypes: types) includes: constraint.
	self assertConstraint: (DRUnionConstraint withTypes: types) includes: constraint.

]

{ #category : #constraints }
DRConditionalConstraintTest >> testIntersectionConstraint [

	| constraint |
	constraint := DRIntersectionConstraint between: 10 and: 20.
	
	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 15).
	self denyConstraint: constraint includes: (DREqualsConstraint withValue: 5).
	self denyConstraint: constraint includes: (DREqualsConstraint withValue: 25).
	
	self denyConstraint: constraint includes: (DRGreaterConstraint withValue: 15).
	self denyConstraint: constraint includes: (DRLessConstraint withValue: 15).
	self denyConstraint: constraint includes: (DRNotEqualsConstraint withValue: 15).
	
	self assertConstraint: constraint includes: (DRIntersectionConstraint between: 10 and: 20).
	self assertConstraint: constraint includes: (DRIntersectionConstraint between: 11 and: 19).
	self denyConstraint: constraint includes: (DRIntersectionConstraint between: 9 and: 20).
	self denyConstraint: constraint includes: (DRIntersectionConstraint between: 10 and: 21).
	self denyConstraint: constraint includes: (DRIntersectionConstraint between: 9 and: 21).

	self denyConstraint: constraint includes: (DRUnionConstraint less: 9 greater: 21).
	self denyConstraint: constraint includes: (DRUnionConstraint less: 11 greater: 19).
	
	self denyConstraint: constraint includes: (DRAnyMaskConstraint withValue: 2).
	self denyConstraint: constraint includes: (DRNoneMaskConstraint withValue: 2)
]

{ #category : #constraints }
DRConditionalConstraintTest >> testLessConstraint [

	| constraint |
	constraint := DRLessConstraint withValue: 10.
	
	self denyConstraint: constraint includes: (DREqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DREqualsConstraint withValue: 11).
	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 9).
	self denyConstraint: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRGreaterConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRLessConstraint withValue: 11).
	self assertConstraint: constraint includes: (DRLessConstraint withValue: 9).
	self assertConstraint: constraint includes: (DRLessConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRNotEqualsConstraint withValue: 10).
]

{ #category : #constraints }
DRConditionalConstraintTest >> testLessOrEqualsConstraint [

	| constraint |
	constraint := DRLessOrEqualsConstraint withValue: 10.
	
	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DREqualsConstraint withValue: 11).
	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 9).
	self denyConstraint: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self assertConstraint: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRGreaterConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRLessConstraint withValue: 11).
	self assertConstraint: constraint includes: (DRLessConstraint withValue: 9).
	self assertConstraint: constraint includes: (DRLessConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRNotEqualsConstraint withValue: 10).
]

{ #category : #constraints }
DRConditionalConstraintTest >> testMaskConstraint [

	| constraint |
	constraint := DRAnyMaskConstraint withValue: 3. "..-1-2-3-5-6-7-10-.."

	self assert: (constraint hasValue: 1).
	self assert: (constraint hasValue: 2).
	self assert: (constraint hasValue: 3).
	self deny: (constraint hasValue: 4).
	self assert: (constraint hasValue: 5).

	self assertConstraint: constraint includes: (DRAnyMaskConstraint withValue: 1).
	self assertConstraint: constraint includes: (DRAnyMaskConstraint withValue: 2).
	self assertConstraint: constraint includes: constraint.
	self denyConstraint: constraint includes: (DRAnyMaskConstraint withValue: 4).
	self denyConstraint: constraint includes: (DRAnyMaskConstraint withValue: 5).
	self denyConstraint: constraint includes: (DRAnyMaskConstraint withValue: 6).
	self denyConstraint: constraint includes: (DRAnyMaskConstraint withValue: 7).
	self assertConstraint: (DRAnyMaskConstraint withValue: 7) includes: constraint.
	
	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 1).
	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 2).
	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 3).
	self denyConstraint: constraint includes: (DREqualsConstraint withValue: 4).
	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 5).

	self denyConstraint: constraint includes: (DRGreaterConstraint withValue: 0).
	self denyConstraint: constraint includes: (DRLessConstraint withValue: 0).
	self denyConstraint: constraint includes: (DRNotEqualsConstraint withValue: 0).
	self denyConstraint: constraint includes: (DRNoneMaskConstraint withValue: 1).

	self denyConstraint: constraint includes: (DRIntersectionConstraint between: 1 and: 10).
	self denyConstraint: constraint includes: (DRUnionConstraint less: 1 greater: 10).


]

{ #category : #constraints }
DRConditionalConstraintTest >> testNotEqualsConstraint [

	| constraint |
	constraint := DRNotEqualsConstraint withValue: 10.
	
	self denyConstraint: constraint includes: (DREqualsConstraint withValue: 10).
	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 11).
	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 9).
	
	self denyConstraint: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	
	self assertConstraint: constraint includes: (DRGreaterConstraint withValue: 10).
	self assertConstraint: constraint includes: (DRGreaterConstraint withValue: 11).
	self denyConstraint: constraint includes: (DRGreaterConstraint withValue: 9).
	
	self assertConstraint: constraint includes: (DRLessConstraint withValue: 10).
	self assertConstraint: constraint includes: (DRLessConstraint withValue: 9).
	self denyConstraint: constraint includes: (DRLessConstraint withValue: 11).
	
	self assertConstraint: constraint includes: (DRNotEqualsConstraint withValue: 10).
	
	self assertConstraint: constraint includes: (DRIntersectionConstraint between: 5 and: 10).
	self denyConstraint: constraint includes: (DRIntersectionConstraint between: 5 and: 15).
	self assertConstraint: constraint includes: (DRIntersectionConstraint between: 10 and: 15).
	
	self assertConstraint: constraint includes: (DRUnionConstraint less: 10 greater: 20).
	self assertConstraint: constraint includes: (DRUnionConstraint less: 10 greater: 10).
	self denyConstraint: constraint includes: (DRUnionConstraint less: 15 greater: 20).
	
	self assertConstraint: constraint includes: (DRAnyMaskConstraint withValue: 1).
	self assertConstraint: constraint includes: (DRNoneMaskConstraint withValue: 2).

]

{ #category : #constraints }
DRConditionalConstraintTest >> testNotEqualsObjectConstraint [

	| constraint |
	constraint := DRNotEqualsObjectConstraint withValue: nil.
	
	self assertConstraint: constraint includes: constraint.
	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 20).
	self denyConstraint: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRGreaterConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRLessConstraint withValue: 10).
	self denyConstraint: constraint includes: (DRNotEqualsConstraint withValue: 10).
	self denyConstraint: constraint includes: (DREqualsConstraint withValue: nil).
]

{ #category : #constraints }
DRConditionalConstraintTest >> testNotMaskConstraint [

	| constraint |
	constraint := DRNoneMaskConstraint withValue: 3. "..-0-4-8-.."

	self deny: (constraint hasValue: 1).
	self deny: (constraint hasValue: 2).
	self deny: (constraint hasValue: 3).
	self assert: (constraint hasValue: 4).
	self deny: (constraint hasValue: 5).
	
	self denyConstraint: constraint includes: (DRNoneMaskConstraint withValue: 1).
	self denyConstraint: constraint includes: (DRNoneMaskConstraint withValue: 2).
	self assertConstraint: constraint includes: constraint.
	self denyConstraint: constraint includes: (DRNoneMaskConstraint withValue: 4).
	self denyConstraint: constraint includes: (DRNoneMaskConstraint withValue: 5).
	self denyConstraint: constraint includes: (DRNoneMaskConstraint withValue: 6).
	self assertConstraint: constraint includes: (DRNoneMaskConstraint withValue: 7).
	
	self denyConstraint: constraint includes: (DREqualsConstraint withValue: 3).
	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 4).
	self denyConstraint: constraint includes: (DREqualsConstraint withValue: 5).

	self denyConstraint: constraint includes: (DRGreaterConstraint withValue: 0).
	self denyConstraint: constraint includes: (DRLessConstraint withValue: 0).
	self denyConstraint: constraint includes: (DRNotEqualsConstraint withValue: 0).
	self denyConstraint: constraint includes: (DRAnyMaskConstraint withValue: 1).

	self denyConstraint: constraint includes: (DRIntersectionConstraint between: 1 and: 10).
	self denyConstraint: constraint includes: (DRUnionConstraint less: 1 greater: 10).

]

{ #category : #tests }
DRConditionalConstraintTest >> testNotSupportedCases [

	<expectedFailure>
	self
		assertConstraint: (DRAnyMaskConstraint withValue: 2) "..-2-3-.."
		includes: (DRIntersectionConstraint between: 1 and: 4) "|1(xxxx)4|".
		
	self
		assertConstraint: (DRUnionConstraint less: 4 greater: 5) "xx)4||5(xx"
		includes: (DRAnyMaskConstraint withValue: 2) "..-2-3-6-7-..".
		
	self
		assertConstraint: (DRNoneMaskConstraint withValue: 2) "..-4-5-.."
		includes: (DRIntersectionConstraint between: 3 and: 6) "3-6".
		
	self
		assertConstraint: (DRUnionConstraint less: 2 greater: 3) "xx)2||3(xx"
		includes: (DRNoneMaskConstraint withValue: 2) "..-0-1-4-5-..".

]

{ #category : #constraints }
DRConditionalConstraintTest >> testUnionConstraint [

	| constraint |
	constraint := DRUnionConstraint less: 10 greater: 20.
	
	self denyConstraint: constraint includes: (DREqualsConstraint withValue: 15).
	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 5).
	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 25).
	
	self denyConstraint: constraint includes: (DRGreaterConstraint withValue: 15).
	self assertConstraint: constraint includes: (DRGreaterConstraint withValue: 20).
	self denyConstraint: constraint includes: (DRGreaterConstraint withValue: 5).
	
	self denyConstraint: constraint includes: (DRLessConstraint withValue: 15).
	self assertConstraint: constraint includes: (DRLessConstraint withValue: 5).
	self denyConstraint: constraint includes: (DRLessConstraint withValue: 25).

	self denyConstraint: constraint includes: (DRNotEqualsConstraint withValue: 15).
	
	self denyConstraint: constraint includes: (DRIntersectionConstraint between: 10 and: 20).
	self denyConstraint: constraint includes: (DRIntersectionConstraint between: 5 and: 15).
	self denyConstraint: constraint includes: (DRIntersectionConstraint between: 15 and: 25).
	self assertConstraint: constraint includes: (DRIntersectionConstraint between: 20 and: 25).
	self assertConstraint: constraint includes: (DRIntersectionConstraint between: 5 and: 10).

	self assertConstraint: constraint includes: (DRUnionConstraint less: 10 greater: 20).
	self denyConstraint: constraint includes: (DRUnionConstraint less: 11 greater: 19).
	self assertConstraint: constraint includes: (DRUnionConstraint less: 9 greater: 21).

	self denyConstraint: constraint includes: (DRAnyMaskConstraint withValue: 2).
	self denyConstraint: constraint includes: (DRNoneMaskConstraint withValue: 2)
]
