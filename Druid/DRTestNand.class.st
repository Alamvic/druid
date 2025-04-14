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

{ #category : 'testing' }
DRTestNand >> isTestNand [

	^true
]

{ #category : 'transforming' }
DRTestNand >> negated [

	^ DRTestAnd new
]

{ #category : 'printing' }
DRTestNand >> opcode [

	^ 'Nand'
]

{ #category : 'converting' }
DRTestNand >> reversed [

	^ self
]

{ #category : 'SCCP' }
DRTestNand >> sccpOperateOnLattice: operand and: operand2 [

	^ (operand and: operand2) not
]
