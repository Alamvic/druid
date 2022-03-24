Class {
	#name : #DRDeadPathElimination,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRDeadPathElimination >> applyTo: cfg [

	| newSucessors deadBranches |
	self generatePaths: cfg.

	"Can exist many paths for the same branch"
	deadBranches := cfg deadPaths values
		                collect: [ :path | 
		                path first source -> path first destination ]
		                as: Set.

	deadBranches
		select: [ :association | 
			cfg isDeadBranch: association key to: association value ]
		thenDo: [ :association | 
			| source destination |
			source := association key.
			destination := association value.

			newSucessors := source successors copyWithout: destination.
			newSucessors ifEmpty: [ source removeFromCFG ] ifNotEmpty: [ 
				1 haltIf: [ newSucessors size > 1 ].
				source jumpTo: newSucessors first ].

			self generatePaths: cfg ]
]

{ #category : #'as yet unclassified' }
DRDeadPathElimination >> generatePaths: cfg [
	cfg clearEdges.
	(DRPathFactory withCFG: cfg) generatePaths
]
