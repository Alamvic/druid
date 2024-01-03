Class {
	#name : #DRStagingTest,
	#superclass : #DRDruidTest,
	#category : #'Druid-Tests-Staging'
}

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
DRStagingTest >> testStageCopyConstant [

	| cfg b0 copy b1 |
	cfg := DRControlFlowGraph new.
	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlockWith: [ :b | copy := b copy: 1 ].	
	b0 jumpTo: b1.
	
	DRStager applyTo: cfg.
	
	self assert: copy isStaged
]
