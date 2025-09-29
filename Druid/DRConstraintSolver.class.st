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

{ #category : 'initialization' }
DRConstraintSolver >> initialize [

	phiReplacements := Dictionary new.
]

{ #category : 'solving' }
DRConstraintSolver >> isSatisfiable: aDRPiNode [ 
	^ self subclassResponsibility
]

{ #category : 'solving' }
DRConstraintSolver >> isSatisfiable: aDRPiNode replacingPhi: phi for: phiOp [

	^ self
		  isSatisfiable: aDRPiNode
		  replacingPhis: (Dictionary newFrom: { (phi -> phiOp) })
]

{ #category : 'solving' }
DRConstraintSolver >> isSatisfiable: aDRPiNode replacingPhis: aDictionary [

	| retval |
	phiReplacements := aDictionary.
	retval := self isSatisfiable: aDRPiNode.
	phiReplacements := Dictionary new.

	^ retval
]
