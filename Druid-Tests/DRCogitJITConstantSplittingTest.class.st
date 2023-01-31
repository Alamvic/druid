Class {
	#name : #DRCogitJITConstantSplittingTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRCogitJITConstantSplittingTest >> testSplitConstantAddition [

	| cfg basicBlock r0 |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | 
		              r0 := block add: 1 to: 2 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitJITConstantSplitting new applyTo: cfg.

	"Since in cogit we cannot express operations with all constant operands,
	We need to split instructions in two.
	E.g.,
	
	A := 1 + 2
	=>
	B := 1
	A := B + 2
	"

	self assert: basicBlock first isCopy.
	self assert: basicBlock first operand1 value equals: 1.

	self assert: basicBlock second operand1 value equals: basicBlock first.
	self assert: basicBlock second operand2 value equals: 2
]

{ #category : #tests }
DRCogitJITConstantSplittingTest >> testSplitObjectReferenceAddition [

	| cfg basicBlock r0 nilObject |
	cfg := DRControlFlowGraph new.
	nilObject := DRNilObject new.
	basicBlock := cfg newBasicBlockWith: [ :block | 
		              r0 := block add: nilObject to: 2 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitJITConstantSplitting new applyTo: cfg.

	"Since in cogit we cannot express operations with all constant operands,
	We need to split instructions in two.
	E.g.,
	
	A := nilObject + 2
	=>
	B := nilObject
	A := B + 2
	"

	self assert: basicBlock first isCopy.
	self assert: basicBlock first operand1 equals: nilObject.

	self assert: basicBlock second operand1 value equals: basicBlock first.
	self assert: basicBlock second operand2 value equals: 2
]
