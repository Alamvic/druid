Class {
	#name : 'DRConstantConstraintSolver',
	#superclass : 'DRConstraintSolver',
	#instVars : [
		'visited'
	],
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'as yet unclassified' }
DRConstantConstraintSolver >> collectConstraintsFrom: aDRValue [

	(aDRValue isNumberValue or: aDRValue value isBooleanValue) ifTrue: [
		^ DREqualsConstraint withValue: aDRValue ].

	visited
		addIfNotPresent: aDRValue
		ifPresentDo: [ ^ DRFullConstraint new ].

	aDRValue isPhiFunction ifTrue: [
		^ DRUnionConstraint constraints:
			  (aDRValue operands collect: [ :op | self collectConstraintsFrom: op ]) ].

	aDRValue isPiNode ifTrue: [
		| constantConstraint |
		constantConstraint := aDRValue constraint asConstantConstraint
			                      ifNil: [ ^ DRFullConstraint new ].
		^ (self collectConstraintsFrom: aDRValue operand) intersectionWith: constantConstraint ].

	aDRValue isCopy ifTrue: [
		^ self collectConstraintsFrom: aDRValue operand ].

	^ DRFullConstraint new
]

{ #category : 'testing' }
DRConstantConstraintSolver >> isSatisfiable: aDRPiNode [
	| collectedConstraint piNodeConstraint |
	
	piNodeConstraint := (aDRPiNode constraint asConstantConstraint) ifNil: [ ^true ].

	visited := Set new.
	collectedConstraint := self collectConstraintsFrom: aDRPiNode operand.

	^ (collectedConstraint negated includes: piNodeConstraint) not.
]
