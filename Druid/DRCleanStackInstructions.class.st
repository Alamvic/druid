Class {
	#name : #DRCleanStackInstructions,
	#superclass : #DRStackOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRCleanStackInstructions >> doApply: cfg [

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
		aStack isEmpty ifTrue: [ ^ self ].
		aDRInstruction stackDelta timesRepeat: [
			| pushInstruction |
			pushInstruction := aStack pop.
			pushInstruction removeFromCFG ].

		aDRInstruction removeFromCFG ].

	aDRInstruction isLoadStackValue ifTrue: [ "aDRInstruction isFlushStack ifTrue: [ 1 halt ]"
		| index |
		index := aDRInstruction operand1 simpleConstantFold.
		index isNumber ifTrue: [
			| pushInstruction |
			pushInstruction := aStack at: index + 1 ifAbsent: [ ^ self ]. "Zero based"

			"Replace by the pushed value"
			aDRInstruction replaceByMaybeValue: pushInstruction operand1 ] ]
]
