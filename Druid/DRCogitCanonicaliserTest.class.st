Class {
	#name : #DRCogitCanonicaliserTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

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
