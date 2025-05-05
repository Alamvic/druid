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

	visited addIfNotPresent: aDRValue ifPresentDo: [ ^DRFullConstraint new ].
	
	aDRValue isPhiFunction ifTrue: [
		^ DRUnionConstraint constraints:
			 (aDRValue operands collect: [ :op | self collectConstraintsFrom: op ])
	].

	aDRValue isPiNode ifTrue: [
		aDRValue constraint constantValue isNumber ifFalse: [ ^DRFullConstraint new ].
		^ (self collectConstraintsFrom: aDRValue operand) intersectionWith: aDRValue constraint
	].

	aDRValue isCopy ifTrue: [
		^ self collectConstraintsFrom: aDRValue operand
	].

	aDRValue isNumberValue ifTrue: [
		^ DREqualsConstraint withValue: aDRValue
	].

	^ DRFullConstraint new
]

{ #category : 'initialization' }
DRConstantConstraintSolver >> initialize [

	visited := Set new.

]

{ #category : 'testing' }
DRConstantConstraintSolver >> isSatisfiable: aDRPiNode [
	| collectedConstraint |
	collectedConstraint := self collectConstraintsFrom: aDRPiNode operand.
	^ (collectedConstraint negated includes: aDRPiNode constraint) not.
]
