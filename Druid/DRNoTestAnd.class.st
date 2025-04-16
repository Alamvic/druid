Class {
	#name : 'DRNoTestAnd',
	#superclass : 'DRCondition',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRNoTestAnd >> acceptVisitor: aVisitor withBranch: parentBranch [

	^ aVisitor visitTestNand: self inBranch: parentBranch
]

{ #category : 'testing' }
DRNoTestAnd >> isNoTestAnd [

	^true
]

{ #category : 'transforming' }
DRNoTestAnd >> negated [

	^ DRTestAnd new
]

{ #category : 'printing' }
DRNoTestAnd >> opcode [

	^ 'Nand'
]

{ #category : 'converting' }
DRNoTestAnd >> reversed [

	^ self
]

{ #category : 'SCCP' }
DRNoTestAnd >> sccpOperateOnLattice: operand and: operand2 [

	^ (operand and: operand2) not
]
