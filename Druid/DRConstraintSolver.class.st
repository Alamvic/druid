Class {
	#name : 'DRConstraintSolver',
	#superclass : 'Object',
	#instVars : [
		'phiReplacements'
	],
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'as yet unclassified' }
DRConstraintSolver >> initialize [

	phiReplacements := Dictionary new.
]

{ #category : 'as yet unclassified' }
DRConstraintSolver >> isSatisfiable: aDRPiNode [ 
	^ self subclassResponsibility
]

{ #category : 'as yet unclassified' }
DRConstraintSolver >> isSatisfiable: aDRPiNode replacingPhi: phi for: phiOp [

	^ self
		  isSatisfiable: aDRPiNode
		  replacingPhis: (Dictionary newFrom: { (phi -> phiOp) })
]

{ #category : 'as yet unclassified' }
DRConstraintSolver >> isSatisfiable: aDRPiNode replacingPhis: aDictionary [

	| retval |
	phiReplacements := aDictionary.
	retval := self isSatisfiable: aDRPiNode.
	phiReplacements := Dictionary new.

	^ retval
]
