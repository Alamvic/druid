Class {
	#name : #DRConditionalConstraintTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #assertions }
DRConditionalConstraintTest >> assert: aConstraint includes: includedConstraint [

	self assert: (aConstraint includes: includedConstraint)
]

{ #category : #assertions }
DRConditionalConstraintTest >> deny: aConstraint includes: includedConstraint [

	self deny: (aConstraint includes: includedConstraint)
]

{ #category : #constraints }
DRConditionalConstraintTest >> testEmptyConstraint [

	| constraint |
	constraint := DREmptyConstraint new.
	
	self assert: constraint includes: (DREmptyConstraint new).
	self deny: constraint includes: (DREqualsConstraint withValue: 10).
	self deny: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self deny: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	self deny: constraint includes: (DRGreaterConstraint withValue: 10).
	self deny: constraint includes: (DRLessConstraint withValue: 10).
	self deny: constraint includes: (DRNotEqualsConstraint withValue: 10).
	self deny: constraint includes: (DRMaskConstraint withValue: 10).
	self deny: constraint includes: (DRNotMaskConstraint withValue: 10).
	self deny: constraint includes: (DRFullConstraint new).
	

	self assert: (DREqualsConstraint withValue: 10) includes: constraint.
	self assert: (DRGreaterOrEqualsConstraint withValue: 10) includes: constraint.
	self assert: (DRLessOrEqualsConstraint withValue: 10) includes: constraint.
	self assert: (DRGreaterConstraint withValue: 10) includes: constraint.
	self assert: (DRLessConstraint withValue: 10) includes: constraint.
	self assert: (DRNotEqualsConstraint withValue: 10) includes: constraint.
	self assert: (DRMaskConstraint withValue: 10) includes: constraint.
	self assert: (DRNotMaskConstraint withValue: 10) includes: constraint.
	self assert: (DRFullConstraint new) includes: constraint.
]

{ #category : #constraints }
DRConditionalConstraintTest >> testEqualsConstraint [

	| constraint |
	constraint := DREqualsConstraint withValue: 10.
	
	self assert: constraint includes: (DREqualsConstraint withValue: 10).
	self deny: constraint includes: (DREqualsConstraint withValue: 20).
	self deny: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self deny: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	self deny: constraint includes: (DRGreaterConstraint withValue: 10).
	self deny: constraint includes: (DRLessConstraint withValue: 10).
	self deny: constraint includes: (DRNotEqualsConstraint withValue: 10).
]

{ #category : #constraints }
DRConditionalConstraintTest >> testGreaterConstraint [

	| constraint |
	constraint := DRGreaterConstraint withValue: 10.
	
	self deny: constraint includes: (DREqualsConstraint withValue: 10).
	self assert: constraint includes: (DREqualsConstraint withValue: 11).
	self deny: constraint includes: (DREqualsConstraint withValue: 9).
	
	self deny: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self deny: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	
	self assert: constraint includes: (DRGreaterConstraint withValue: 10).
	self assert: constraint includes: (DRGreaterConstraint withValue: 11).
	self deny: constraint includes: (DRGreaterConstraint withValue: 9).
	
	self deny: constraint includes: (DRLessConstraint withValue: 10).
	
	self deny: constraint includes: (DRNotEqualsConstraint withValue: 10).
	
	self assert: constraint includes: (DRIntersectionConstraint between: 10 and: 20).
	self deny: constraint includes: (DRIntersectionConstraint between: 5 and: 20).
	self deny: constraint includes: (DRIntersectionConstraint between: 5 and: 9).
	
	self deny: constraint includes: (DRUnionConstraint less: 5 greater: 15).
]

{ #category : #constraints }
DRConditionalConstraintTest >> testGreaterOrEqualsConstraint [

	| constraint |
	constraint := DRGreaterOrEqualsConstraint withValue: 10.
	
	self assert: constraint includes: (DREqualsConstraint withValue: 10).
	self assert: constraint includes: (DREqualsConstraint withValue: 11).
	self deny: constraint includes: (DREqualsConstraint withValue: 9).
	self assert: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self deny: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	self assert: constraint includes: (DRGreaterConstraint withValue: 10).
	self assert: constraint includes: (DRGreaterConstraint withValue: 11).
	self deny: constraint includes: (DRGreaterConstraint withValue: 9).
	self deny: constraint includes: (DRLessConstraint withValue: 10).
	self deny: constraint includes: (DRNotEqualsConstraint withValue: 10).
]

{ #category : #constraints }
DRConditionalConstraintTest >> testIntersectionConstraint [

	| constraint |
	constraint := DRIntersectionConstraint between: 10 and: 20.
	
	self assert: constraint includes: (DREqualsConstraint withValue: 15).
	self deny: constraint includes: (DREqualsConstraint withValue: 5).
	self deny: constraint includes: (DREqualsConstraint withValue: 25).
	
	self deny: constraint includes: (DRGreaterConstraint withValue: 15).
	self deny: constraint includes: (DRLessConstraint withValue: 15).
	self deny: constraint includes: (DRNotEqualsConstraint withValue: 15).
	
	self assert: constraint includes: (DRIntersectionConstraint between: 10 and: 20).
	self assert: constraint includes: (DRIntersectionConstraint between: 11 and: 19).
	self deny: constraint includes: (DRIntersectionConstraint between: 9 and: 20).
	self deny: constraint includes: (DRIntersectionConstraint between: 10 and: 21).
	self deny: constraint includes: (DRIntersectionConstraint between: 9 and: 21).

	self deny: constraint includes: (DRUnionConstraint less: 9 greater: 21).
	self deny: constraint includes: (DRUnionConstraint less: 11 greater: 19).
	
	self deny: constraint includes: (DRMaskConstraint withValue: 2).
	self deny: constraint includes: (DRNotMaskConstraint withValue: 2)
]

{ #category : #constraints }
DRConditionalConstraintTest >> testLessConstraint [

	| constraint |
	constraint := DRLessConstraint withValue: 10.
	
	self deny: constraint includes: (DREqualsConstraint withValue: 10).
	self deny: constraint includes: (DREqualsConstraint withValue: 11).
	self assert: constraint includes: (DREqualsConstraint withValue: 9).
	self deny: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self deny: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	self deny: constraint includes: (DRGreaterConstraint withValue: 10).
	self deny: constraint includes: (DRLessConstraint withValue: 11).
	self assert: constraint includes: (DRLessConstraint withValue: 9).
	self assert: constraint includes: (DRLessConstraint withValue: 10).
	self deny: constraint includes: (DRNotEqualsConstraint withValue: 10).
]

{ #category : #constraints }
DRConditionalConstraintTest >> testLessOrEqualsConstraint [

	| constraint |
	constraint := DRLessOrEqualsConstraint withValue: 10.
	
	self assert: constraint includes: (DREqualsConstraint withValue: 10).
	self deny: constraint includes: (DREqualsConstraint withValue: 11).
	self assert: constraint includes: (DREqualsConstraint withValue: 9).
	self deny: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self assert: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	self deny: constraint includes: (DRGreaterConstraint withValue: 10).
	self deny: constraint includes: (DRLessConstraint withValue: 11).
	self assert: constraint includes: (DRLessConstraint withValue: 9).
	self assert: constraint includes: (DRLessConstraint withValue: 10).
	self deny: constraint includes: (DRNotEqualsConstraint withValue: 10).
]

{ #category : #constraints }
DRConditionalConstraintTest >> testMaskConstraint [

	| constraint |
	constraint := DRMaskConstraint withValue: 3. "..-1-2-3-5-6-7-10-.."

	self assert: (constraint hasValue: 1).
	self assert: (constraint hasValue: 2).
	self assert: (constraint hasValue: 3).
	self deny: (constraint hasValue: 4).
	self assert: (constraint hasValue: 5).

	self assert: constraint includes: (DRMaskConstraint withValue: 1).
	self assert: constraint includes: (DRMaskConstraint withValue: 2).
	self assert: constraint includes: constraint.
	self deny: constraint includes: (DRMaskConstraint withValue: 4).
	self deny: constraint includes: (DRMaskConstraint withValue: 5).
	self deny: constraint includes: (DRMaskConstraint withValue: 6).
	self deny: constraint includes: (DRMaskConstraint withValue: 7).
	self assert: (DRMaskConstraint withValue: 7) includes: constraint.
	
	self assert: constraint includes: (DREqualsConstraint withValue: 1).
	self assert: constraint includes: (DREqualsConstraint withValue: 2).
	self assert: constraint includes: (DREqualsConstraint withValue: 3).
	self deny: constraint includes: (DREqualsConstraint withValue: 4).
	self assert: constraint includes: (DREqualsConstraint withValue: 5).

	self deny: constraint includes: (DRGreaterConstraint withValue: 0).
	self deny: constraint includes: (DRLessConstraint withValue: 0).
	self deny: constraint includes: (DRNotEqualsConstraint withValue: 0).
	self deny: constraint includes: (DRNotMaskConstraint withValue: 1).

	self deny: constraint includes: (DRIntersectionConstraint between: 1 and: 10).
	self deny: constraint includes: (DRUnionConstraint less: 1 greater: 10).


]

{ #category : #constraints }
DRConditionalConstraintTest >> testNotEqualsConstraint [

	| constraint |
	constraint := DRNotEqualsConstraint withValue: 10.
	
	self deny: constraint includes: (DREqualsConstraint withValue: 10).
	self assert: constraint includes: (DREqualsConstraint withValue: 11).
	self assert: constraint includes: (DREqualsConstraint withValue: 9).
	
	self deny: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self deny: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	
	self assert: constraint includes: (DRGreaterConstraint withValue: 10).
	self assert: constraint includes: (DRGreaterConstraint withValue: 11).
	self deny: constraint includes: (DRGreaterConstraint withValue: 9).
	
	self assert: constraint includes: (DRLessConstraint withValue: 10).
	self assert: constraint includes: (DRLessConstraint withValue: 9).
	self deny: constraint includes: (DRLessConstraint withValue: 11).
	
	self assert: constraint includes: (DRNotEqualsConstraint withValue: 10).
	
	self assert: constraint includes: (DRIntersectionConstraint between: 5 and: 10).
	self deny: constraint includes: (DRIntersectionConstraint between: 5 and: 15).
	self assert: constraint includes: (DRIntersectionConstraint between: 10 and: 15).
	
	self assert: constraint includes: (DRUnionConstraint less: 10 greater: 20).
	self assert: constraint includes: (DRUnionConstraint less: 10 greater: 10).
	self deny: constraint includes: (DRUnionConstraint less: 15 greater: 20).
	
	self assert: constraint includes: (DRMaskConstraint withValue: 1).
	self assert: constraint includes: (DRNotMaskConstraint withValue: 2).

]

{ #category : #constraints }
DRConditionalConstraintTest >> testNotMaskConstraint [

	| constraint |
	constraint := DRNotMaskConstraint withValue: 3. "..-0-4-8-.."

	self deny: (constraint hasValue: 1).
	self deny: (constraint hasValue: 2).
	self deny: (constraint hasValue: 3).
	self assert: (constraint hasValue: 4).
	self deny: (constraint hasValue: 5).
	
	self deny: constraint includes: (DRNotMaskConstraint withValue: 1).
	self deny: constraint includes: (DRNotMaskConstraint withValue: 2).
	self assert: constraint includes: constraint.
	self deny: constraint includes: (DRNotMaskConstraint withValue: 4).
	self deny: constraint includes: (DRNotMaskConstraint withValue: 5).
	self deny: constraint includes: (DRNotMaskConstraint withValue: 6).
	self assert: constraint includes: (DRNotMaskConstraint withValue: 7).
	
	self deny: constraint includes: (DREqualsConstraint withValue: 3).
	self assert: constraint includes: (DREqualsConstraint withValue: 4).
	self deny: constraint includes: (DREqualsConstraint withValue: 5).

	self deny: constraint includes: (DRGreaterConstraint withValue: 0).
	self deny: constraint includes: (DRLessConstraint withValue: 0).
	self deny: constraint includes: (DRNotEqualsConstraint withValue: 0).
	self deny: constraint includes: (DRMaskConstraint withValue: 1).

	self deny: constraint includes: (DRIntersectionConstraint between: 1 and: 10).
	self deny: constraint includes: (DRUnionConstraint less: 1 greater: 10).

]

{ #category : #tests }
DRConditionalConstraintTest >> testNotSupportedCases [

	<expectedFailure>
	self
		assert: (DRMaskConstraint withValue: 2) "..-2-3-.."
		includes: (DRIntersectionConstraint between: 1 and: 4) "|1(xxxx)4|".
		
	self
		assert: (DRUnionConstraint less: 4 greater: 5) "xx)4||5(xx"
		includes: (DRMaskConstraint withValue: 2) "..-2-3-6-7-..".
		
	self
		assert: (DRNotMaskConstraint withValue: 2) "..-4-5-.."
		includes: (DRIntersectionConstraint between: 3 and: 6) "3-6".
		
	self
		assert: (DRUnionConstraint less: 2 greater: 3) "xx)2||3(xx"
		includes: (DRNotMaskConstraint withValue: 2) "..-0-1-4-5-..".

]

{ #category : #constraints }
DRConditionalConstraintTest >> testUnionConstraint [

	| constraint |
	constraint := DRUnionConstraint less: 10 greater: 20.
	
	self deny: constraint includes: (DREqualsConstraint withValue: 15).
	self assert: constraint includes: (DREqualsConstraint withValue: 5).
	self assert: constraint includes: (DREqualsConstraint withValue: 25).
	
	self deny: constraint includes: (DRGreaterConstraint withValue: 15).
	self assert: constraint includes: (DRGreaterConstraint withValue: 20).
	self deny: constraint includes: (DRGreaterConstraint withValue: 5).
	
	self deny: constraint includes: (DRLessConstraint withValue: 15).
	self assert: constraint includes: (DRLessConstraint withValue: 5).
	self deny: constraint includes: (DRLessConstraint withValue: 25).

	self deny: constraint includes: (DRNotEqualsConstraint withValue: 15).
	
	self deny: constraint includes: (DRIntersectionConstraint between: 10 and: 20).
	self deny: constraint includes: (DRIntersectionConstraint between: 5 and: 15).
	self deny: constraint includes: (DRIntersectionConstraint between: 15 and: 25).
	self assert: constraint includes: (DRIntersectionConstraint between: 20 and: 25).
	self assert: constraint includes: (DRIntersectionConstraint between: 5 and: 10).

	self assert: constraint includes: (DRUnionConstraint less: 10 greater: 20).
	self deny: constraint includes: (DRUnionConstraint less: 11 greater: 19).
	self assert: constraint includes: (DRUnionConstraint less: 9 greater: 21).

	self deny: constraint includes: (DRMaskConstraint withValue: 2).
	self deny: constraint includes: (DRNotMaskConstraint withValue: 2)
]
