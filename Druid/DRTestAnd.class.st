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

{ #category : 'testing' }
DRTestAnd >> isTestAnd [

	^true
]

{ #category : 'transforming' }
DRTestAnd >> negated [

	^ DRTestNand new
]

{ #category : 'printing' }
DRTestAnd >> opcode [

	^ 'And'
]

{ #category : 'converting' }
DRTestAnd >> reversed [

	^ self
]

{ #category : 'SCCP' }
DRTestAnd >> sccpOperateOnLattice: operand and: operand2 [

	^ operand and: operand2
]
