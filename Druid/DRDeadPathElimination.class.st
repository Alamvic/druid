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
1halt.
	[ cfg deadPaths isNotEmpty and: change ] whileTrue: [ 
		change := false.
		(cfg deadPaths values collect: [ :path |  path first source -> path first destination]) asSet
		do:  [ :association | |source destination|
			source := association key.
			destination := association value.
			targetBlock := source.

			((cfg edgesFrom: targetBlock to: destination) 
				 allSatisfy: [ :e | e isDead ])
				ifTrue: [ 
					change := true.
					newSucessors := targetBlock successors copyWithout: destination.
					newSucessors
						ifEmpty: [ targetBlock removeFromCFG ]
						ifNotEmpty: [ 
							1 haltIf: [ newSucessors size > 1 ].
							targetBlock jumpTo: newSucessors first ] ]
				ifFalse: [ 
					targetBlock predecessors size > 1 ifTrue: [ 
						targetBlock tailDuplicate.
						change := true ]
					ifFalse: [ 1halt. ]
					 ].

			self generatePaths: cfg ] ]
]

{ #category : #'as yet unclassified' }
DRDeadPathElimination >> generatePaths: cfg [
	cfg clearEdges.
	(DRPathFactory withCFG: cfg) generatePaths
]
