Class {
	#name : 'DRPiConstantDeadBranchElimination',
	#superclass : 'DRPiDeadBranchElimination',
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'testing' }
DRPiConstantDeadBranchElimination >> isReachable: aDRBasicBlock [

	^ aDRBasicBlock piNodes allSatisfy: [ :pi |
		  DRConstantConstraintSolver new isSatisfiable: pi ]
]
