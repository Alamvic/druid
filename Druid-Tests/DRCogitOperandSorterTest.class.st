Class {
	#name : #DRCogitOperandSorterTest,
	#superclass : #DRIRTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRCogitOperandSorterTest >> testSortAddition [

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

{ #category : #tests }
DRCogitOperandSorterTest >> testSplitAdditionWithConstants [

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

{ #category : #tests }
DRCogitOperandSorterTest >> testSplitAdditionWithObjectReferences [

	| cfg r1 |
	cfg := self setUpCFG: 2.

	"R1 := 1 + R1"
	r1 := cfg b1 add: DRInterpreterReference nilObject to: 2.

	"The sorter should split one constant to register"
	DRCogitOperandSorter new applyTo: cfg.

	self assert: r1 operands first isCopy.
	self assert: r1 operands first operand1 equals: 2 asDRValue.
	self assert: r1 operands second isInterpreterReference
]
