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

	optimisations add: DRCogitStackCoalescing new.
	4 timesRepeat: [
		optimisations add: DRBranchCollapse new.
		optimisations add: DRFailureCodeBasedTailDuplication new.
		optimisations add: (DRSCCP then: DRDeadBlockElimination).
		optimisations add: (DRPhiSimplication then: DRCopyPropagation).
		optimisations add: DRCogitStackCanonicaliser new.
		optimisations add:
			(DRDeadCodeElimination then: DRRedundantCopyElimination).
		optimisations add: DRCleanControlFlow new.
		optimisations add: DRTailDuplicateStagedExpressions new.
		optimisations add:
			(DRDeadEdgeSplitting then: DRDeadBranchElimination).
		optimisations add: (DRPhiSimplication then: DRCopyPropagation) ].

	"Last Phase, make code coggable"
	optimisations add: (DRCogitStackCanonicaliser then: DRSCCP).
	optimisations add: DRCogitOperandSorter new.
	optimisations add: DRCogitStager new.
	optimisations add: DRCogitOperandSorter new.
	
	optimisations add: DRCogitSendMarshallInsertion new.
	optimisations add: DRCogitStackToRegisterFlushHoisting new.
	optimisations add: DRDeadCodeElimination new.
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
