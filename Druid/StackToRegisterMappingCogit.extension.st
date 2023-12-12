Extension { #name : #StackToRegisterMappingCogit }

{ #category : #'*Druid' }
StackToRegisterMappingCogit class >> druidNewCodeGenerator [

	^ DRCogitStackToRegisterMappingGenerator new
]

{ #category : #'*Druid' }
StackToRegisterMappingCogit class >> druidNewOptimizationList [

	| optimisations |
	optimisations := super druidNewOptimizationList.
	optimisations add: DRCogitSendMarshallInsertion new.
	optimisations add: DRCogitStackToRegisterFlushHoisting new.
	^ optimisations
]

{ #category : #'*Druid' }
StackToRegisterMappingCogit class >> druidNewRegisterAllocator [

	^ DRCogitDynamicLinearScanRegisterAllocator new
]
