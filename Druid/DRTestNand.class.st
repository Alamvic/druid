Class {
	#name : 'DRTestNand',
	#superclass : 'DRCondition',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRTestNand >> acceptVisitor: aVisitor withBranch: parentBranch [

	^ aVisitor visitTestNand: self inBranch: parentBranch
]

{ #category : 'transforming' }
DRTestNand >> negated [

	^ DRTestAnd new
]

{ #category : 'converting' }
DRTestNand >> reversed [
	"Returns the same condition but with reversed operands: Nand x y --> Nand y x"
	^ DRTestNand new
]

{ #category : 'SCCP' }
DRTestNand >> sccpOperateOnLattice: operand and: operand2 [

	^ (operand and: operand2) not
]
