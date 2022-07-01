Class {
	#name : #DRCogitOperandSorterTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRCogitOperandSorterTest >> testSortAddition [

	| cfg basicBlock r1 r2 |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		r1 := block copy: 3.
		"R2 := 1 + R1"
		r2 := block add: 1 to: r1 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	"The sorter should turn around the addition"
	DRCogitOperandSorter new applyTo: cfg.
	
	self assert: basicBlock second operands first equals: r1.
	self assert: basicBlock second operands second equals: 1 asDRValue.
]
