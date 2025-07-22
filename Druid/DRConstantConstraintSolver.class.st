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
		| val |
		val := aDRValue constraint constantValue asDRValue
			       simpleConstantFold value.
		(val isNumber or: val isBooleanValue)
			ifTrue: [
				^ (self collectConstraintsFrom: aDRValue operand) intersectionWith: aDRValue constraint ]
			ifFalse: [ ^ DRFullConstraint new ] ].

	aDRValue isCopy ifTrue: [
		^ self collectConstraintsFrom: aDRValue operand ].

	^ DRFullConstraint new
]

{ #category : 'testing' }
DRConstantConstraintSolver >> isSatisfiable: aDRPiNode [
	| collectedConstraint |
	
	(aDRPiNode constraint constantValue isNumber or: aDRPiNode constraint constantValue value isBooleanValue) ifFalse: [ ^true ].

	visited := Set new.
	collectedConstraint := self collectConstraintsFrom: aDRPiNode operand.

	^ (collectedConstraint negated includes: aDRPiNode constraint) not.
]
