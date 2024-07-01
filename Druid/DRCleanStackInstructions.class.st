Class {
	#name : #DRCleanStackInstructions,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRCleanStackInstructions >> applyTo: cfg [

	cfg reversePostOrderBlocks do: [ :b | self interpretAndOptimise: b ].

]

{ #category : #accessing }
DRCleanStackInstructions >> interpretAndOptimise: basicBlock [

	| stack |
	stack := Stack new.
	basicBlock instructionsDo: [ :i |
		self interpretAndOptimise: i on: stack ]
]

{ #category : #optimising }
DRCleanStackInstructions >> interpretAndOptimise: aDRInstruction on: aStack [

	aDRInstruction isPush ifTrue: [ "Record the instruction"
		aStack push: aDRInstruction ].

	aDRInstruction isPop ifTrue: [
		| pushInstruction |
		aStack isEmpty ifTrue: [ ^ self ].
		pushInstruction := aStack pop.

		"Clean both instructions"
		aDRInstruction removeFromCFG.
		pushInstruction removeFromCFG ].

	aDRInstruction isLoadStackValue ifTrue: [
		| index |
		index := aDRInstruction operand1 simpleConstantFold.
		index isNumber ifTrue: [
			| pushInstruction |
			pushInstruction := aStack at: index + 1 ifAbsent: [ ^ self ]. "Zero based"

			"Replace by the pushed value"
			aDRInstruction replaceBy: pushInstruction operand1 ] ].

	aDRInstruction isFlushStack ifTrue: [ 1 halt ]
]
