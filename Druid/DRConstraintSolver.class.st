Class {
	#name : 'DRConstraintSolver',
	#superclass : 'Object',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'as yet unclassified' }
DRConstraintSolver >> isSatisfiable: aDRPiNode [ 
	^ self subclassResponsibility
]
