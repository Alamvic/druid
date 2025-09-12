Class {
	#name : 'DRConstantConstraintSolver',
	#superclass : 'DRConstraintSolver',
	#instVars : [
		'visited',
		'phiReplacement'
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
		^ (phiReplacement isNil or: [ phiReplacement key ~= aDRValue ]) ifTrue: [
			DRUnionConstraint constraints:
			  (aDRValue operands collect: [ :op | self collectConstraintsFrom: op ])
		] ifFalse: [
			self collectConstraintsFrom: phiReplacement value
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

{ #category : 'testing' }
DRConstantConstraintSolver >> isConstraint: piNodeConstraint satisfiableWith: collectedConstraint [

	^ (piNodeConstraint negated includes: collectedConstraint) not
]

{ #category : 'testing' }
DRConstantConstraintSolver >> isSatisfiable: aDRPiNode [

	| constraint |

	constraint := aDRPiNode constraint asConstantConstraint ifNil: [ ^ true ].
	^ self isSatisfiable: constraint onVar: aDRPiNode operand
]

{ #category : 'testing' }
DRConstantConstraintSolver >> isSatisfiable: piNodeConstraint onVar: constrainedVariable [

	| collectedConstraint |

	visited := Set new.
	collectedConstraint := self collectConstraintsFrom:
		                       constrainedVariable.

	^ self
		  isConstraint: piNodeConstraint
		  satisfiableWith: collectedConstraint
]

{ #category : 'testing' }
DRConstantConstraintSolver >> isSatisfiable: aDRPiNode replacingPhi: phi for: phiOp [

	| retval |
	phiReplacement := phi -> phiOp.
	retval := self isSatisfiable: aDRPiNode.
	phiReplacement := nil.

	^ retval
]
