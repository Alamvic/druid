Class {
	#name : #DRDeadPathElimination,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRDeadPathElimination >> applyTo: cfg [

	| blockToSplit change newSucessors |
	self generatePaths: cfg.

	change := cfg deadPaths isNotEmpty.

	[ change ] whileTrue: [ 
		cfg deadPaths valuesDo: [ :path | 
			change := false.
			blockToSplit := path first source.

			blockToSplit predecessors size > 1
				ifTrue: [ 
					blockToSplit tailDuplicate.
					change := true ]
				ifFalse: [ 
					(cfg edgesFrom: blockToSplit to: path first destination) size = 1 
						ifTrue: [ 
							newSucessors := blockToSplit successors copyWithout: path first destination.
							newSucessors
				ifEmpty: [ blockToSplit removeFromCFG ]
				ifNotEmpty: [ 
					1haltIf: [ newSucessors size > 1 ].
					blockToSplit jumpTo: newSucessors first ].
						] ].
			"			
"

			self generatePaths: cfg ].
	 ]
]

{ #category : #'as yet unclassified' }
DRDeadPathElimination >> generatePaths: cfg [
	cfg clearPaths.
	(DRPathFactory withCFG: cfg) generatePaths
]
