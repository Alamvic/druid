Class {
	#name : 'DRCogitOperandSorterTest',
	#superclass : 'DRIRTest',
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
}

{ #category : 'tests' }
DRCogitOperandSorterTest >> testExtractFirstOperandInCopy [

	| cfg r1 |
	cfg := self setUpCFG: 2.

	"R1 := 1 + R1"
	r1 := cfg b1 add: 1 to: 2.

	"The sorter should split one constant to register"
	DRCogitOperandSorter new applyTo: cfg.

	self assert: r1 operands first isCopy.
	self assert: r1 operands first operand1 equals: 2 asDRValue.
	self assert: r1 operands second equals: 1 asDRValue
]

{ #category : 'tests' }
DRCogitOperandSorterTest >> testSortConstantAndPhysicalRegister [

	| cfg basicBlock firstRegister physicalRegister |
	physicalRegister := DRPhysicalGeneralPurposeRegister name: 'Receiver'.
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		              | r1 |
		              "R1 := 1 + Receiver"
		              r1 := block add: 1 to: physicalRegister ].
	cfg initialBasicBlock jumpTo: basicBlock.

	firstRegister := DRPhysicalGeneralPurposeRegister name: 'PR1'.

	"The sorter should leave constants at the end"
	DRCogitOperandSorter new applyTo: cfg.

	self assert: basicBlock instructions first isCopy.
	self assert: basicBlock instructions first operand1 equals: physicalRegister.
	self assert: basicBlock instructions second isAdd.
	self assert: basicBlock instructions second operand2 equals: 1 asDRValue
]

{ #category : 'tests' }
DRCogitOperandSorterTest >> testSortConstantAndPhysicalRegisterNonCommutative [

	| cfg basicBlock specialRegister |

	specialRegister := DRPhysicalGeneralPurposeRegister name: 'Receiver'.
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 |
		r0 := block greater: 1 than: specialRegister ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	"The sorter should leave constants at the end"
	DRCogitOperandSorter new applyTo: cfg.

	self assert: basicBlock instructions first isCopy. 
	self assert: basicBlock instructions first operand1 equals: 1 asDRValue. 
	self assert: basicBlock instructions second isSetConditionCode.
	self assert: basicBlock instructions second operand2 equals: specialRegister
]

{ #category : 'tests' }
DRCogitOperandSorterTest >> testSortConstantAndSSARegister [

	| cfg r1 r2 |
	cfg := self setUpCFG: 2.

	r1 := cfg b1 copy: 3.
	"R2 := 1 + R1"
	r2 := cfg b1 add: 1 to: r1 .
	
	"The sorter should turn around the addition"
	DRCogitOperandSorter new applyTo: cfg.
	
	self assert: r2 operands first equals: r1.
	self assert: r2 operands second equals: 1 asDRValue.
]

{ #category : 'tests' }
DRCogitOperandSorterTest >> testTestSpecialRegisterDoesNotModifyInstructionInversed [

	| cfg basicBlock specialRegister |

	specialRegister := DRPhysicalGeneralPurposeRegister name: 'Receiver'.
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 |
		r0 := block anyMask: 1 with: specialRegister ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	"The sorter should leave constants at the end"
	DRCogitOperandSorter new applyTo: cfg.

	self assert: basicBlock instructions first isCopy. 
	self assert: basicBlock instructions first operand1 equals: 1 asDRValue. 
	self assert: basicBlock instructions second isSetConditionCode.
	self assert: basicBlock instructions second operand2 equals: specialRegister
]
