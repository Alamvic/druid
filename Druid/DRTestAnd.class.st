Class {
	#name : 'DRTestAnd',
	#superclass : 'DRCondition',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRTestAnd >> acceptVisitor: aVisitor withBranch: parentBranch [

	^ aVisitor visitTestAnd: self inBranch: parentBranch
]

{ #category : 'transforming' }
DRTestAnd >> negated [

	^ DRTestNand new
]

{ #category : 'converting' }
DRTestAnd >> reversed [
	"Returns the same condition but with reversed operands: And x y --> And y x"
	^ DRTestAnd new
]

{ #category : 'SCCP' }
DRTestAnd >> sccpOperateOnLattice: operand and: operand2 [

	^ operand and: operand2
]
