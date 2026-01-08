Class {
	#name : 'DRPhiSimplication',
	#superclass : 'DROptimisation',
	#instVars : [
		'shouldFoldCopies'
	],
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'asserting' }
DRPhiSimplication class >> shouldFoldCopies: aBoolean [
	^ self new shouldFoldCopies: aBoolean; yourself.
]

{ #category : 'operations' }
DRPhiSimplication >> applyTo: aDRControlFlowGraph [
	
	aDRControlFlowGraph phiFunctions do: [ :i |
		| operands operandsWithoutMyself |
		"If all operands are the same (excluding myself),
		I can be replaced by a copy instruction"
		operands := (i operands collect: [ :op | self foldCopies: op ]) asSet.
		operandsWithoutMyself := operands copyWithout: i.
		((operandsWithoutMyself size = 1) and: [ operandsWithoutMyself unique result isNoResult not ]) ifTrue: [
			| copy |
			copy := DRCopy operands: { operandsWithoutMyself unique } result: i result.
			i replaceBy: copy
			] 
		]
]

{ #category : 'testing' }
DRPhiSimplication >> canBeAppliedIn: aDRControlFlowGraph [

	^ aDRControlFlowGraph phiFunctions notEmpty
]

{ #category : 'testing' }
DRPhiSimplication >> foldCopies: aDRInstruction [
	| res |
	
	shouldFoldCopies ifFalse: [ ^aDRInstruction ].

	res := aDRInstruction.
	[ res isCopy ] whileTrue: [ res := res operand ].

	^ res.
]

{ #category : 'testing' }
DRPhiSimplication >> initialize [

	shouldFoldCopies := true.
]

{ #category : 'testing' }
DRPhiSimplication >> shouldFoldCopies: aBoolean [

	shouldFoldCopies := aBoolean
]
