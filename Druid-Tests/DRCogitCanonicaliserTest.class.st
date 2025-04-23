Class {
	#name : 'DRCogitCanonicaliserTest',
	#superclass : 'DROptimisationTest',
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
}

{ #category : 'tests' }
DRCogitCanonicaliserTest >> buildCFGWithEightShape [
	| cfg b0 b1 b2 b3 b4 b5 b6 b7 jump jump2 r1 r2 phi cond jump3 |
	cfg := DRControlFlowGraphForTesting new.

	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock.
	b5 := cfg newBasicBlock.
	b6 := cfg newBasicBlock.
	b7 := cfg newBasicBlock.
	jump := cfg instructionFactory jumpIf: false.
	jump2 := cfg instructionFactory jumpIf: false.

	b7 endInstruction: DRReturn new.
	b0 jumpTo: b1.
	jump operand1 isInstruction ifTrue: [
		b1 addInstruction: jump operand1 ].
	b1 endInstruction: jump.
	jump trueBranch: b2.
	b2 addPredecessor: b1.
	jump falseBranch: b3.
	b3 addPredecessor: b1.

	r1 := b2 loadReceiver.
	b2 jumpTo: b4.
	r2 := b3 copy: 17 asDRValue.
	b3 jumpTo: b4.

	jump2 operand1 isInstruction ifTrue: [
		b4 addInstruction: jump2 operand1 ].
	b4 endInstruction: jump2.
	jump2 trueBranch: b5.
	b5 addPredecessor: b4.
	jump2 falseBranch: b6.
	b6 addPredecessor: b4.

	b5 setReturnValue: 42.
	b5 jumpTo: b7.
	b6 setReturnValue: 10.
	b6 jumpTo: b7.

	phi := b4 phiWith: r1 with: r2.
	cond := b4 lessOrEquals: phi than: 20.
	jump3 := b4 jumpIf: cond to: b6 ifFalseTo: b5.

	b7 beExitBlock.
	
	^ cfg
]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> optimize: cfg [

	cfg applyOptimisation: (DRSCCP then: DRCopyPropagation).
	cfg applyOptimisation: (DRDeadCodeElimination then: DRCleanControlFlow).
	cfg applyOptimisation: DRPhiSimplication new
]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> setUp [

	super setUp.
	optimisation := DRCogitCanonicaliser new
]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testConstantPropagateOnConstantPhiOperands [

	| cfg phi phiOperandsBefore phiOperandsAfter numberOfPhisBefore |
	cfg := self buildCFGWithEightShape.

	phi := cfg phiFunctions unique.

	"We save this data to test against the CFG after we apply the optimization pass that we are testing"
	numberOfPhisBefore := cfg phiFunctions size.
	phiOperandsBefore := phi operands copy.

	"Apply the optimization to test"
	DRCogitCanonicaliser new applyTo: cfg.

	"We check that the number of Phi's in our CFG has doubled"
	self assert: numberOfPhisBefore * 2 equals: cfg phiFunctions size.

	"And that the two operators of the Phi before the optimization, now each of them belongs to a Phi each of one contains only one operator"
	phiOperandsAfter := cfg phiFunctions flatCollect: [ :aPhi |
		                    aPhi operands ].

	self assert:
		(cfg phiFunctions allSatisfy: [ :aPhi | aPhi operands size = 1 ]).

	self assert: phiOperandsBefore equals: phiOperandsAfter
]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testDoNotSimplifyAdditionOfAdditionIfMultipleUsers [

	| cfg basicBlock |
	cfg := DRControlFlowGraph new.
	"(A + Constant ) + B => (A + B) + Constant"
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 r3 |
		r0 := block loadFramePointer.
		r1 := block loadArgument: 1.
		"Two additions of constants over a variable value"
		r2 := block add: r0 to: 17.
		r3 := block add: r2 to: r1.
		"Store the value so it is not treated as dead code"
		block storeSInt64: r3 at: 888.
		block storeSInt64: r2 at: 888 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	DRCogitCanonicaliser new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.

	"Nothing should have changed because r2 has two users, so no transformation is available"
	self assert: basicBlock instructions third operand2 value equals: 17.
]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testDoNotSimplifyPushLoadBaseOffsetInNormalCanonicalizer [

	| cfg basicBlock |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		r0 := block loadFramePointer.
		r1 := block add: r0 to: 17.
		"load the value so it is not treated as dead code"
		r2 := block loadSInt64: r1.
		block push: r2 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	"The normal canonicalizer does not this simplification"
	DRCogitCanonicaliser new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.

	"The two additions should be simplified to a single one"
	self assert: basicBlock instructions size equals: 4.
	
	self assert: basicBlock instructions second isLoad.
	self assert: basicBlock instructions second operand1 offset value equals: 17.
	self assert: basicBlock instructions second operand1 base value equals: basicBlock instructions first.
	
	self assert: basicBlock instructions third isPush.
	self assert: basicBlock instructions third operand1 equals: basicBlock instructions second.
]

{ #category : 'tests' }
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
	DRSCCP new applyTo:cfg.
	DRDeadCodeElimination new applyTo: cfg.

	"The two additions should be simplified to a single one"
	self assert: basicBlock instructions size equals: 4.
	self assert: basicBlock instructions second operand1 value equals: 42 "17 + 25".
]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testSimplifyAdditionOfAdditionNoConstant [

	| cfg basicBlock |
	cfg := DRControlFlowGraph new.
	"(A + Constant ) + B => (A + B) + Constant"
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 r3 |
		r0 := block loadFramePointer.
		r1 := block loadArgument: 1.
		"Two additions of constants over a variable value"
		r2 := block add: r0 to: 17.
		r3 := block add: r2 to: r1.
		"Store the value so it is not treated as dead code"
		block storeSInt64: r3 at: 888 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitCanonicaliser new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.

	"We should still keep all instructions, but operands moved around"
	self assert: basicBlock instructions size equals: 6.
	self assert: basicBlock instructions fourth operand2 value equals: 17.
]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testSimplifyAdditionOfSubtraction [

	| cfg basicBlock |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		r0 := block loadFramePointer.
		"Two additions of constants over a variable value"
		"(r0 - 17) + 25 => r0 + 8"
		r1 := block subtract: 17 from: r0.
		r2 := block add: r1 to: 25.
		"Store the value so it is not treated as dead code"
		block storeSInt64: r2 at: 888 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitCanonicaliser new applyTo: cfg.
	DRSCCP new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.

	"The two additions should be simplified to a single one"
	self assert: basicBlock instructions size equals: 4.
	self assert: basicBlock instructions second operand1 value equals: 8 "- 17 + 25".
]

{ #category : 'tests' }
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

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testSimplifyAndEqualsZero [
	"Convert TestEqualsThan(And(a, b), 0) into  TestNand(a, b) and collapse into branch"
	| cfg basicBlock b1 b2 |
	cfg := DRControlFlowGraph new.
	b1 := cfg newBasicBlockWith: [ :block | block storeSInt64: 1 at: 888 ].
	b2 := cfg newBasicBlockWith: [ :block | block storeSInt64: 2 at: 888 ].
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		r0 := block loadFramePointer.
		r1 := block and: r0 with: 1.
		r2 := block equals: r1 to: 0.
		block jumpIf: r2 to: b1 ifFalseTo: b2.
	].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitCanonicaliser new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.
	
	self assert: basicBlock instructions size equals: 2.
	self assert: basicBlock instructions second isConditionalBranch.
	self assert: basicBlock instructions second condition class equals: DRNoTestAnd.
	self assert: basicBlock instructions second operand2 value equals: 1.
]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testSimplifyAndNotEqualsZero [
	"Convert TestNotEqualsThan(And(a, b), 0) into  TestAnd(a, b) and collapse into branch"
	| cfg basicBlock b1 b2 |
	cfg := DRControlFlowGraph new.
	b1 := cfg newBasicBlockWith: [ :block | block storeSInt64: 1 at: 888 ].
	b2 := cfg newBasicBlockWith: [ :block | block storeSInt64: 2 at: 888 ].
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		r0 := block loadFramePointer.
		r1 := block and: r0 with: 1.
		r2 := block notEquals: r1 to: 0.
		block jumpIf: r2 to: b1 ifFalseTo: b2.
	].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitCanonicaliser new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.
	
	self assert: basicBlock instructions size equals: 2.
	self assert: basicBlock instructions second isConditionalBranch.
	self assert: basicBlock instructions second condition class equals: DRTestAnd.
	self assert: basicBlock instructions second operand2 value equals: 1.
]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testSimplifyBitAndEqualsZero [
	"Convert TestEqualsThan(BitAnd(a, b), 0) into  TestNoMask(a, b) and collapse into branch"
	| cfg basicBlock b1 b2 |
	cfg := DRControlFlowGraph new.
	b1 := cfg newBasicBlockWith: [ :block | block storeSInt64: 1 at: 888 ].
	b2 := cfg newBasicBlockWith: [ :block | block storeSInt64: 2 at: 888 ].
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		r0 := block loadFramePointer.
		r1 := block bitAnd: r0 with: 17.
		r2 := block equals: r1 to: 0.
		block jumpIf: r2 to: b1 ifFalseTo: b2.
	].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitCanonicaliser new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.

	self assert: basicBlock instructions size equals: 2.
	self assert: basicBlock instructions second isConditionalBranch.
	self assert: basicBlock instructions second condition class equals: DRNoTestMask.
	self assert: basicBlock instructions second operand2 value equals: 17.
]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testSimplifyBitAndNotEqualsZero [
	"Convert TestNotEqualsThan(BitAnd(a, b), 0) into  TestMask(a, b) and collapse into branch"
	| cfg basicBlock b1 b2 |
	cfg := DRControlFlowGraph new.
	b1 := cfg newBasicBlockWith: [ :block | block storeSInt64: 1 at: 888 ].
	b2 := cfg newBasicBlockWith: [ :block | block storeSInt64: 2 at: 888 ].
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		r0 := block loadFramePointer.
		r1 := block bitAnd: r0 with: 17.
		r2 := block notEquals: r1 to: 0.
		block jumpIf: r2 to: b1 ifFalseTo: b2.
	].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitCanonicaliser new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.
	
	self assert: basicBlock instructions size equals: 2.
	self assert: basicBlock instructions second isConditionalBranch.
	self assert: basicBlock instructions second condition class equals: DRTestMask.
	self assert: basicBlock instructions second operand2 value equals: 17.
]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testSimplifyBitAndOfBitAnd [

	| cfg basicBlock r0 instruction |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r1 r2 |
		r0 := block loadFramePointer.
		"Two bit ANDs of constants over a variable value"
		r1 := block bitAnd: r0 with: 17.
		r2 := block bitAnd: r1 with: 25.
		"Store the value so it is not treated as dead code"
		block storeSInt64: r2 at: 888 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitCanonicaliser new applyTo: cfg.
	DRSCCP new applyTo: cfg.

	instruction := basicBlock instructions detect: [ :inst | inst isBitAnd ].

	self assert: instruction operand1 value equals: (17 bitAnd: 25). "constant"
	self assert: instruction operand2 equals: r0
	
]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testSimplifyBitOrOfBitOr [

	| cfg basicBlock r0 instruction |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r1 r2 |
		r0 := block loadFramePointer.
		"Two Bit ORs of constants over a variable value"
		r1 := block bitOr: r0 with: 17.
		r2 := block bitOr: r1 with: 25.
		"Store the value so it is not treated as dead code"
		block storeSInt64: r2 at: 888 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitCanonicaliser new applyTo: cfg.
	DRSCCP new applyTo: cfg.

	instruction := basicBlock instructions detect: [ :inst | inst isBitOr ].

	self assert: instruction operand1 value equals: (17 bitOr: 25). "constant"
	self assert: instruction operand2 equals: r0
	
]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testSimplifyBitXorOfBitXor [

	| cfg basicBlock r0 instruction |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r1 r2 |
		r0 := block loadFramePointer.
		"Two Bit ORs of constants over a variable value"
		r1 := block bitXor: r0 with: 17.
		r2 := block bitXor: r1 with: 25.
		"Store the value so it is not treated as dead code"
		block storeSInt64: r2 at: 888 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitCanonicaliser new applyTo: cfg.
	DRSCCP new applyTo: cfg.

	instruction := basicBlock instructions detect: [ :inst | inst isBitXor ].

	self assert: instruction operand1 value equals: (17 bitXor: 25). "constant"
	self assert: instruction operand2 equals: r0

]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testSimplifyBranchIfConstanteable [

	| cfg condition initialBlockCount |
	cfg := self
		       setUpCFGWithConditionalWithPhiBetween: DRInterpreterReference trueObject
		       and: DRInterpreterReference falseObject.
	condition := cfg b4 equals: cfg phi to: DRInterpreterReference trueObject.
	cfg b4 jumpIf: condition to: cfg newBasicBlock ifFalseTo: cfg newBasicBlock.

	DRBranchCollapse new applyTo: cfg.

	initialBlockCount := cfg blocks size.
	DRCogitCanonicaliser new applyTo: cfg.

	self assert: cfg blocks size equals: initialBlockCount + 3.
	self assert: cfg phi operands size equals: 1
]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testSimplifyBranchIfSetConditionCode [

	| cfg phi resultPhi |
	cfg := self generateDruidIRFor: #primitiveOr.

	"Phi with complex jumps"
	phi := cfg mergeBlocks first phiFunctions first.
	self assert: phi operands size equals: 2.
	self assert: phi operand1 isSetConditionCode.

	resultPhi := cfg mergeBlocks second phiFunctions first.

	cfg applyOptimisation: optimisation.

	"Simple phi"
	self assert: phi operands size equals: 1.
	self assert: phi operand1 isSetConditionCode.

	"Clean simple phis to copies"
	cfg applyOptimisation: DRPhiSimplication new.

	"Just result phi survived"
	self assert: cfg phiFunctions size equals: 1.
	self assert: cfg phiFunctions first equals: resultPhi

]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testSimplifyCopyLoadReceiver [

	| cfg basicBlock |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 |
		r0 := block loadReceiver.
		r1 := block copy: r0.
		r1 result: (DRPhysicalGeneralPurposeRegister name: 'ReceiverResultReg') ].
	cfg initialBasicBlock jumpTo: basicBlock.

	"The stack canonicalizer does this simplification"
	DRCogitCanonicaliser new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.

	"
	R0 := load receiver
	ReceiverResultReg := R0.
	=>
	"
	self assert: basicBlock instructions size equals: 1.
	self assert: basicBlock instructions first isNoop
]

{ #category : 'tests' }
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
	DRSCCP new applyTo: cfg.
	DRCogitCanonicaliser new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.

	"The two additions should be collapsed and moved as base,offset of the load"
	self assert: basicBlock instructions second isLoad.
	self assert: basicBlock instructions second address base equals: basicBlock instructions first.
	self assert: basicBlock instructions second address offset value equals: 42
]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testSimplifyLoadBaseNegativeOffset [

	| cfg basicBlock |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		r0 := block loadFramePointer.
		r1 := block subtract: 17 from: r0.
		"load the value so it is not treated as dead code"
		r2 := block loadSInt64: r1.
		block storeSInt64: r2 at: 888 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitCanonicaliser new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.

	"The two additions should be simplified to a single one"
	self assert: basicBlock instructions size equals: 4.
	self assert: basicBlock instructions second isLoad.
	self assert: basicBlock instructions second address offset value equals: -17.
	self assert: basicBlock instructions second address base value equals: basicBlock instructions first.
]

{ #category : 'tests' }
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

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testSimplifyMultiplyOfMultiply [

	| cfg basicBlock r0 instruction |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r1 r2 |
		r0 := block loadFramePointer.
		"Two bit ANDs of constants over a variable value"
		r1 := block multiply: r0 by: 17.
		r2 := block multiply: r1 by: 25.
		"Store the value so it is not treated as dead code"
		block storeSInt64: r2 at: 888 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitCanonicaliser new applyTo: cfg.
	DRSCCP new applyTo: cfg.

	instruction := basicBlock instructions detect: [ :inst | inst isMultiply ].

	self assert: instruction operand1 value equals: (17 * 25). "constant"
	self assert: instruction operand2 equals: r0
	
]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testSimplifyPushLoadBaseOffset [

	| cfg basicBlock |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		r0 := block loadFramePointer.
		r1 := block add: r0 to: 17.
		"load the value so it is not treated as dead code"
		r2 := block loadSInt64: r1.
		block push: r2 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	"The stack canonicalizer does this simplification"
	DRCogitStackCanonicaliser new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.

	"The two additions should be simplified to a single one"
	self assert: basicBlock instructions size equals: 3.
	self assert: basicBlock instructions second isPush.
	self assert: basicBlock instructions second operand1 offset value equals: 17.
	self assert: basicBlock instructions second operand1 base value equals: basicBlock instructions first.
]

{ #category : 'tests' }
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

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testSimplifyStoreBaseNegativeOffset [

	| cfg basicBlock |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 |
		r0 := block loadFramePointer.
		r1 := block subtract: 17 from: r0.
		"load the value so it is not treated as dead code"
		block storeSInt64: 888 at: r1 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitCanonicaliser new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.

	"The two additions should be simplified to a single one"
	self assert: basicBlock instructions size equals: 3.
	self assert: basicBlock instructions second isStore.
	self assert: basicBlock instructions second address offset value equals: -17.
	self assert: basicBlock instructions second address base value equals: basicBlock instructions first.
]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testSimplifyStoreBaseOffset [

	| cfg basicBlock |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 |
		r0 := block loadFramePointer.
		r1 := block add: r0 to: 17.
		"load the value so it is not treated as dead code"
		block storeSInt64: 888 at: r1 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitCanonicaliser new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.

	"The two additions should be simplified to a single one"
	self assert: basicBlock instructions size equals: 3.
	self assert: basicBlock instructions second isStore.
	self assert: basicBlock instructions second address offset value equals: 17.
	self assert: basicBlock instructions second address base value equals: basicBlock instructions first.
]

{ #category : 'tests' }
DRCogitCanonicaliserTest >> testSimplifySubtractionOfAddition [

	| cfg basicBlock |
	cfg := DRControlFlowGraph new.
	"(A + Constant1) - Constant2 => A + (Constant1 - Constant2)"
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		r0 := block loadFramePointer.
		r1 := block add: r0 to: 17.
		r2 := block subtract: 25 from: r1.
		"Store the value so it is not treated as dead code"
		block storeSInt64: r2 at: 888 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitCanonicaliser new applyTo: cfg.
	DRSCCP new applyTo:cfg.
	DRDeadCodeElimination new applyTo: cfg.

	"Constants operations should be simplified"
	self assert: basicBlock instructions size equals: 4.
	self assert: basicBlock instructions second isAdd.
	self assert: basicBlock instructions second operand2 value equals: -8 "17 - 25".
]
