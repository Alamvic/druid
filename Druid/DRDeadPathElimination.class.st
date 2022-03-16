Class {
	#name : #DRDeadPathElimination,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRDeadPathElimination >> applyTo: cfg [

| blockToSplit newSucessors path |

		self generatePaths: cfg.

"	[ cfg deadPaths isNotEmpty ] whileTrue: [ "
		
		path := cfg deadPaths values last.
		blockToSplit := path first source.
		newSucessors := blockToSplit successors copyWithout:
			                path first destination.

		1 haltIf: [ newSucessors size ~= 1 ].

		blockToSplit tailDuplicate.

		newSucessors
			ifEmpty: [ blockToSplit removeFromCFG ]
			ifNotEmpty: [ blockToSplit jumpTo: newSucessors first ].


		self generatePaths: cfg 
		"]"
]

{ #category : #'as yet unclassified' }
DRDeadPathElimination >> generatePaths: cfg [
	cfg clearPaths.
	(DRPathFactory withCFG: cfg) generatePaths
]
