Class {
	#name : 'DRPiConstantMessageSplitting',
	#superclass : 'DRPiMessageSplitting',
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRPiConstantMessageSplitting >> solverFor: cfg [

	^ DRConstantConstraintSolver new
]
