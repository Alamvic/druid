Extension { #name : #StackToRegisterMappingCogit }

{ #category : #'*Druid' }
StackToRegisterMappingCogit class >> druidNewCodeGenerator [

	^ DRCogitStackToRegisterMappingGenerator new
]

{ #category : #'*Druid' }
StackToRegisterMappingCogit class >> druidNewOptimizationList [

	| optimisations |
	optimisations := OrderedCollection new.
	optimisations add: (DRCleanControlFlow then: DRCopyPropagation).

	"Basic set of optimizations twice for primitives"
	3 timesRepeat: [
		optimisations add: DRBranchCollapse new.
	"	optimisations add: DRStoreTemporaryStagedLoop new."
		optimisations add: DRFailureCodeBasedTailDuplication new.
		optimisations add: (DRSCCPWithStaging then: DRDeadBlockElimination).
		optimisations add: DRCopyPropagation new.
		optimisations add: DRPhiSimplication new.
		optimisations add: DRCogitStackCanonicaliser new.
		optimisations add:
			(DRDeadCodeElimination then: DRRedundantCopyElimination).
		optimisations add: DRCleanControlFlow new.
		optimisations add: DRTailDuplicateStagedExpressions new.
		optimisations add:
			(DRDeadEdgeSplitting then: DRDeadBranchElimination) ].

	"Last Phase, make code coggable"
	optimisations add: DRCogitStager new.
	optimisations add: DRCogitOperandSorter new.
	optimisations add: DRCogitSendMarshallInsertion new.
	optimisations add: DRCogitStackToRegisterFlushHoisting new.
	^ optimisations
]

{ #category : #'*Druid' }
StackToRegisterMappingCogit class >> druidNewPrimitiveCodeGenerator [

	^ DRCogitStackToRegisterPrimitiveCodeGenerator new
]

{ #category : #'*Druid' }
StackToRegisterMappingCogit class >> druidNewRegisterAllocator [

	^ DRCogitDynamicLinearScanRegisterAllocator new
]
