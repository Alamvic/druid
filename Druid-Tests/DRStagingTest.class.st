Class {
	#name : 'DRStagingTest',
	#superclass : 'DROptimisationTest',
	#category : 'Druid-Tests-Staging',
	#package : 'Druid-Tests',
	#tag : 'Staging'
}

{ #category : 'tests' }
DRStagingTest >> testDoNotChangeStagedRegister [

	| cfg b0 copy b1 |
	cfg := DRControlFlowGraph new.
	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlockWith: [ :b |
		      copy := b copy: 1.
		      copy result: (DRStagedRegister name: 'stg') ].
	b0 jumpTo: b1.

	DRStager applyTo: cfg.

	self assert: copy isStaged.
	self assert: copy result name equals: 'stg'
]

{ #category : 'tests' }
DRStagingTest >> testDoNotStageAddOfNotStagedInstructions [

	| cfg b0 copy b1 copy2 add |
	cfg := DRControlFlowGraph new.
	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlockWith: [ :b |
		      copy := b copy: 1.
		      copy2 := b load: copy.
		      add := b add: copy to: copy2 ].
	b0 jumpTo: b1.

	DRStager applyTo: cfg.

	self assert: add isStaged not
]

{ #category : 'tests' }
DRStagingTest >> testDoNotStageLoad [

	| cfg b0 copy b1 copy2 add |
	cfg := DRControlFlowGraph new.
	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlockWith: [ :b |
		      copy := b load: 1.
		      copy2 := b copy: 2.
		      add := b add: copy to: copy2 ].
	b0 jumpTo: b1.

	DRStager applyTo: cfg.

	self deny: add isStaged
]

{ #category : 'tests' }
DRStagingTest >> testDoNotStageLoadWithStagedAddress [

	| cfg b0 copy b1 copy2 |
	cfg := DRControlFlowGraph new.
	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlockWith: [ :b |
		      copy := b copy: 1.
		      copy2 := b load: copy ].
	b0 jumpTo: b1.

	DRStager applyTo: cfg.

	self deny: copy2 isStaged
]

{ #category : 'phi-staging' }
DRStagingTest >> testDoNotStagePhiOnNonStageableControlFlow [

	| cfg b0 argument b1 b2 b3 copyOnTop copyOnBranch phi |
	cfg := DRControlFlowGraph new.
	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlockWith: [ :b | 
		copyOnTop := b copy: 17.
		argument := b loadArgument: 1 ].
	b0 jumpTo: b1.

	b2 := cfg newBasicBlockWith: [ :b |
		copyOnBranch := b copy: 42 ].
	b3 := cfg newBasicBlock.
	b1 jumpIf: argument to: b2 ifFalseTo: b3.
	b2 jumpTo: b3.
	
	"The jump in b1 cannot be staged because it depends on an argument"
	"Thus this phi cannot be staged either"
	phi := b3 phiWith: copyOnTop with: copyOnBranch.

	DRStager applyTo: cfg.

	self deny: phi isStaged
]

{ #category : 'tests' }
DRStagingTest >> testInstructionWithStagedRegisterIsNotStagedByDefault [

	| cfg b0 copy b1 |
	cfg := DRControlFlowGraph new.
	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlockWith: [ :b |
		      copy := b copy: 1.
		      copy result: (DRStagedRegister name: 'stg') ].
	b0 jumpTo: b1.

	self deny: copy isStaged
]

{ #category : 'tests' }
DRStagingTest >> testStageAddOfStagedInstructions [

	| cfg b0 copy b1 copy2 add |
	cfg := DRControlFlowGraph new.
	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlockWith: [ :b |
		      copy := b copy: 1.
		      copy2 := b copy: 2.
		      add := b add: copy to: copy2 ].
	b0 jumpTo: b1.

	DRStager applyTo: cfg.

	self assert: add isStaged
]

{ #category : 'tests' }
DRStagingTest >> testStageCopyConstant [

	| cfg b0 copy b1 |
	cfg := DRControlFlowGraph new.
	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlockWith: [ :b | copy := b copy: 1 ].	
	b0 jumpTo: b1.
	
	DRStager applyTo: cfg.
	
	self assert: copy isStaged
]

{ #category : 'phi-staging' }
DRStagingTest >> testStageLoop [

	| cfg b0 b1 phi header increment body exit initialValue |
	cfg := DRControlFlowGraph new.
	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlockWith: [ :b | 
		initialValue := b copy: 0.
	].
	b0 jumpTo: b1.
	header := cfg newBasicBlock.
	b1 jumpTo: header.
	
	phi := header phiWithVariables: {  }.
	body := cfg newBasicBlock.
	exit := cfg newBasicBlock.
	body jumpTo: header.

	increment := body add: 1 to: phi.
	phi joinedVariables: { initialValue . increment }.
	header jumpIf: (header greater: 10 than: phi) to: body ifFalseTo: exit.

	DRStager applyTo: cfg.

	self assert: phi isStaged
]

{ #category : 'phi-staging' }
DRStagingTest >> testStagePhiOnNonStageableNestedSinglePredecessorControlFlow [

	| cfg b0 argument b1 b2 b3 copyOnTop copyOnBranch phi subtrue submerge |
	cfg := DRControlFlowGraph new.
	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlockWith: [ :b | 
		copyOnTop := b copy: 17.
		argument := b loadArgument: 1 ].
	b0 jumpTo: b1.

	b2 := cfg newBasicBlockWith: [ :b |
		copyOnBranch := b copy: 42
	].
	subtrue := cfg newBasicBlock.
	submerge := cfg newBasicBlock.
	b2 jumpIf: argument to: subtrue ifFalseTo: submerge.
	subtrue jumpTo: submerge.
	
	b3 := cfg newBasicBlock.
	b1 jumpIf: copyOnTop to: b2 ifFalseTo: b3.
	submerge jumpTo: b3.
	
	"The jump in b1 can be staged because it depends on a constant"
	"However, inside of the conditional, there is another conditional that cannot be staged!"
	"But this phi can be staged because it does not depend on that control flow edge nor in a non stageable instruction!"
	phi := b3 phiWith: copyOnTop with: copyOnBranch.

	DRStager applyTo: cfg.

	self assert: phi isStaged
]

{ #category : 'phi-staging' }
DRStagingTest >> testStagePhiOnStageableControlFlow [

	| cfg b0 argument b1 b2 b3 copyOnTop copyOnBranch phi |
	cfg := DRControlFlowGraph new.
	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlockWith: [ :b | 
		copyOnTop := b copy: 17.
		argument := b loadArgument: 1 ].
	b0 jumpTo: b1.

	b2 := cfg newBasicBlockWith: [ :b |
		copyOnBranch := b copy: 42 ].
	b3 := cfg newBasicBlock.
	b1 jumpIf: copyOnTop to: b2 ifFalseTo: b3.
	b2 jumpTo: b3.
	
	"The jump in b1 can be staged because it depends on a constant"
	"Thus this phi can be staged too!"
	phi := b3 phiWith: copyOnTop with: copyOnBranch.

	DRStager applyTo: cfg.

	self assert: phi isStaged
]
