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

{ #category : 'solving' }
DRConstantConstraintSolver >> collectConstraintsFrom: aDRValue [

	(aDRValue isNumberValue or: aDRValue value isBooleanValue) ifTrue: [
		^ DREqualsConstraint withValue: aDRValue ].

	visited
		addIfNotPresent: aDRValue
		ifPresentDo: [ ^ DRFullConstraint new ].

	aDRValue isPhiFunction ifTrue: [
		^ phiReplacements at: aDRValue ifPresent: [ :phiReplacement |
			self collectConstraintsFrom: phiReplacement
		] ifAbsent: [
			DRUnionConstraint constraints:
			  (aDRValue operands collect: [ :op | self collectConstraintsFrom: op ])
		].
	].

	aDRValue isPiNode ifTrue: [
		| constantConstraint |
		constantConstraint := aDRValue constraint asConstantConstraint
			                      ifNil: [ ^ DRFullConstraint new ].
		^ (self collectConstraintsFrom: aDRValue operand) intersectionWith: constantConstraint ].

	aDRValue isCopy ifTrue: [
		^ self collectConstraintsFrom: aDRValue operand ].

	^ DRFullConstraint new
]

{ #category : 'solving' }
DRConstantConstraintSolver >> isConstraint: piNodeConstraint satisfiableWith: collectedConstraint [

	^ (piNodeConstraint negated includes: collectedConstraint) not
]

{ #category : 'solving' }
DRConstantConstraintSolver >> isSatisfiable: aDRPiNode [

	| constraint |

	constraint := aDRPiNode constraint asConstantConstraint ifNil: [ ^ true ].
	^ self isSatisfiable: constraint onVar: aDRPiNode operand
]

{ #category : 'solving' }
DRConstantConstraintSolver >> isSatisfiable: piNodeConstraint onVar: constrainedVariable [

	| collectedConstraint |

	visited := Set new.
	collectedConstraint := self collectConstraintsFrom:
		                       constrainedVariable.

	^ self
		  isConstraint: piNodeConstraint
		  satisfiableWith: collectedConstraint
]
