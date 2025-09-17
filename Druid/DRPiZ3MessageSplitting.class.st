Class {
	#name : 'DRPiZ3MessageSplitting',
	#superclass : 'DRPiMessageSplitting',
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRPiZ3MessageSplitting >> solverFor: cfg [

	^ DRZ3ConstraintSolver for: cfg
]
