Extension { #name : #SimpleStackBasedCogit }

{ #category : #'*Druid' }
SimpleStackBasedCogit class >> druidNewCodeGenerator [

	^ DRCogitSimpleStackGenerator new
]

{ #category : #'*Druid' }
SimpleStackBasedCogit class >> druidNewOptimizationList [

	| optimisations |
	optimisations := OrderedCollection new.
	optimisations add: (DRCleanControlFlow then: DRCopyPropagation).

	"Basic set of optimizations twice for primitives"
	3 timesRepeat: [
		optimisations add: DRBranchCollapse new.
		optimisations add: DRFailureCodeBasedTailDuplication new.
		optimisations add: (DRSCCPWithStaging then: DRDeadBlockElimination).
		optimisations add: DRCopyPropagation new.
		optimisations add: DRPhiSimplication new.
		optimisations add: DRCogitCanonicaliser new.
		optimisations add:
			(DRDeadCodeElimination then: DRRedundantCopyElimination).
		optimisations add: DRCleanControlFlow new.
		optimisations add: DRTailDuplicateStagedExpressions new.
		optimisations add:
			(DRDeadEdgeSplitting then: DRDeadBranchElimination) ].

	"Last Phase, make code coggable"
	optimisations add: DRCogitOperandSorter new.
	^ optimisations
]

{ #category : #'*Druid' }
SimpleStackBasedCogit class >> druidNewPrimitiveCodeGenerator [

	^ DRCogitSimpleStackPrimitiveCodeGenerator new
]

{ #category : #'*Druid' }
SimpleStackBasedCogit class >> druidNewRegisterAllocator [

	^ DRCogitLinearScanRegisterAllocator new
]
