Class {
	#name : #DRDeadBranchElimination,
	#superclass : #DRPathOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRDeadBranchElimination >> doApplyWithGeneratedPaths: cfg [

	"Remove first (upper) dead branch in CFG"

	| path |
	cfg deadBranches ifEmpty: [ ^ self ].

	path := cfg deadBranches detectMax: [ :v | v size ].

	self removeBranchFrom: path first source to: path first destination
]

{ #category : #optimising }
DRDeadBranchElimination >> removeBranchFrom: sourceBasicBlock to: destinationBasicBlock [

	| newSucessors |
	newSucessors := sourceBasicBlock successors copyWithout: destinationBasicBlock.
	newSucessors
		ifEmpty: [ sourceBasicBlock removeFromCFG ]
		ifNotEmpty: [ 
			self assert: newSucessors size < 2. "A basic block has 2 successors max"
			sourceBasicBlock jumpTo: newSucessors first ]
]
