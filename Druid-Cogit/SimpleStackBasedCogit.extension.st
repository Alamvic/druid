Extension { #name : 'SimpleStackBasedCogit' }

{ #category : '*Druid-Cogit' }
SimpleStackBasedCogit class >> druidNewCodeGenerator [

	^ DRCogitSimpleStackGenerator new
]

{ #category : '*Druid-Cogit' }
SimpleStackBasedCogit class >> druidNewOptimizationList [

	| optimisations |
	optimisations := OrderedCollection new.
	optimisations add: (DRCleanControlFlow then: DRCopyPropagation).

	"Basic set of optimizations twice for primitives"
	3 timesRepeat: [
		optimisations add: DRBranchCollapse new.
		"optimisations add: DRFailureCodeBasedTailDuplication new."
		optimisations add: (DRSCCP then: DRDeadBlockElimination).

		optimisations add: DRCopyPropagation new.
		optimisations add: DRPhiSimplication new.
		optimisations add: DRCogitCanonicaliser new.
		optimisations add:
			(DRDeadCodeElimination then: DRRedundantCopyElimination).
		optimisations add: DRCleanControlFlow new.
		"optimisations add: DRTailDuplicateStagedExpressions new."
		optimisations add:
			(DRDeadEdgeSplitting then: DRDeadBranchElimination) ].

	"Last Phase, make code coggable"
	optimisations add: DRCogitOperandSorter new.
	optimisations add: DRCogitCanonicaliser new.
	optimisations add: DRCogitStager new.
	optimisations add: DRCogitOperandSorter new.
	optimisations add: DRDeadCodeElimination new.
	^ optimisations
]

{ #category : '*Druid-Cogit' }
SimpleStackBasedCogit class >> druidNewPrimitiveCodeGenerator [

	^ DRCogitSimpleStackPrimitiveCodeGenerator new
]

{ #category : '*Druid-Cogit' }
SimpleStackBasedCogit class >> druidNewRegisterAllocator [

	^ DRCogitLinearScanRegisterAllocator new
]

{ #category : '*Druid-Cogit' }
SimpleStackBasedCogit class >> isSimpleStack [

	^ true
]
