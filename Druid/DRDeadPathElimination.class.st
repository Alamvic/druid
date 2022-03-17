Class {
	#name : #DRDeadPathElimination,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRDeadPathElimination >> applyTo: cfg [

	| targetBlock change newSucessors |
	self generatePaths: cfg.

	change := cfg deadPaths isNotEmpty.

	[ cfg deadPaths isNotEmpty and: change ] whileTrue: [ 
		change := false.
		cfg deadPaths values reversed do: [ :path | 
			targetBlock := path first source.

			((cfg edgesFrom: targetBlock to: path first destination) 
				 allSatisfy: [ :e | e isDead ])
				ifTrue: [ 
					change := true.
					newSucessors := targetBlock successors copyWithout:
						                path first destination.
					newSucessors
						ifEmpty: [ targetBlock removeFromCFG ]
						ifNotEmpty: [ 
							1 haltIf: [ newSucessors size > 1 ].
							targetBlock jumpTo: newSucessors first ] ]
				ifFalse: [ 
					targetBlock predecessors size > 1 ifTrue: [ 
						targetBlock tailDuplicate.
						change := true ] ].

			self generatePaths: cfg ] ]
]

{ #category : #'as yet unclassified' }
DRDeadPathElimination >> generatePaths: cfg [
	cfg clearEdges.
	(DRPathFactory withCFG: cfg) generatePaths
]
