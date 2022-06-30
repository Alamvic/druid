Class {
	#name : #DRDeadPathElimination,
	#superclass : #DRControlFlowOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRDeadPathElimination >> doApply: cfg [

	| newSucessors deadBranches path source destination |
	cfg generatePaths.

	cfg deadBranches ifEmpty: [ ^self ].

	path := cfg deadBranches detectMax: [ :v | v size ].
	source := path first source.
	destination := path first destination.

	newSucessors := source successors copyWithout: destination.
	newSucessors ifEmpty: [ source removeFromCFG ] ifNotEmpty: [ 
		1 haltIf: [ newSucessors size > 1 ].
		source jumpTo: newSucessors first ].

	cfg generatePaths


	"
	""Can exist many paths for the same branch""
	deadBranches := cfg deadPaths values
		                collect: [ :path | 
		                path first source -> path first destination ]
		                as: Set.
1halt.
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

			cfg generatePaths ]"
]
