Class {
	#name : #DRCogitCanonicaliserTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRCogitCanonicaliserTest >> testSimplifyAdditionOfAddition [

	| cfg basicBlock |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		r0 := block loadFramePointer.
		"Two additions of constants over a variable value"
		r1 := block add: r0 to: 17.
		r2 := block add: r1 to: 25.
		"Store the value so it is not treated as dead code"
		block storeSInt64: r2 at: 888 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitCanonicaliser new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.

	"The two additions should be simplified to a single one"
	self assert: basicBlock instructions size equals: 4.
	self assert: basicBlock instructions second operand2 value equals: 42 "17 + 25".
]

{ #category : #tests }
DRCogitCanonicaliserTest >> testSimplifyAdditionOfZero [

	| cfg basicBlock |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 |
		r0 := block loadFramePointer.
		r1 := block add: r0 to: 0.
		"Store the value so it is not treated as dead code"
		block storeSInt64: r1 at: 888 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitCanonicaliser new applyTo: cfg.
	DRCopyPropagation new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.

	"The two additions should be simplified to a single one"
	self assert: basicBlock instructions size equals: 3.
	self assert: basicBlock instructions second isStore.
	self assert: basicBlock instructions second operand1 equals: basicBlock instructions first.
]

{ #category : #tests }
DRCogitCanonicaliserTest >> testSimplifyLoadAdditionOfAddition [

	| cfg basicBlock |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 r3 |
		r0 := block loadFramePointer.
		"Two additions of constants over a variable value"
		r1 := block add: r0 to: 17.
		r2 := block add: r1 to: 25.
		"Store the value so it is not treated as dead code"
		r3 := block loadSInt64: r2.
		block storeSInt64: r3 at: 888 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitCanonicaliser new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.

	"The two additions should be collapsed and moved as base,offset of the load"
	self assert: basicBlock instructions second isLoad.
	self assert: basicBlock instructions second address base equals: basicBlock instructions first.
	self assert: basicBlock instructions second address offset value equals: 42
]

{ #category : #tests }
DRCogitCanonicaliserTest >> testSimplifyLoadBaseOffset [

	| cfg basicBlock |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		r0 := block loadFramePointer.
		r1 := block add: r0 to: 17.
		"load the value so it is not treated as dead code"
		r2 := block loadSInt64: r1.
		block storeSInt64: r2 at: 888 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitCanonicaliser new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.

	"The two additions should be simplified to a single one"
	self assert: basicBlock instructions size equals: 4.
	self assert: basicBlock instructions second isLoad.
	self assert: basicBlock instructions second address offset value equals: 17.
	self assert: basicBlock instructions second address base value equals: basicBlock instructions first.
]

{ #category : #tests }
DRCogitCanonicaliserTest >> testSimplifyReceiverAccess [

	| cfg basicBlock |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 receiver |
		r0 := block loadFramePointer.
		r1 := block copy: (block jitCompileTimeVariable: 'FrameReceiverOffset').
		r2 := block add: r0 to: r1.
		receiver := block loadSInt64: r2.
		block storeSInt64: receiver at: 17 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRSCCP new applyTo: cfg.
	DRCogitCanonicaliser new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.

	self assert: basicBlock instructions size equals: 3.
	self assert: basicBlock instructions first isLoadReceiver.
]
