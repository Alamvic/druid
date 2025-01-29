Class {
	#name : #DRBranchCollapse,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRBranchCollapse >> applyTo: aDRControlFlowGraph [

	aDRControlFlowGraph blocksDo: [ :b | self applyToBlock: b ]
]

{ #category : #'as yet unclassified' }
DRBranchCollapse >> applyToBlock: aDRBasicBlock [

	| setConditionCode replacementBranch |
	(self canBeAppliedToBlock: aDRBasicBlock) ifFalse: [ ^ self ].

	"
	R1 := SetConditionCode a > b
	Jump if R1 = true

	=>
	
	Jump if a > b
	"

	setConditionCode := aDRBasicBlock endInstruction operand1 simpleConstantFold.
	replacementBranch := DRBranchIfCondition operands: {
			                     setConditionCode condition.
			                     setConditionCode operand1.
			                     setConditionCode operand2 }.
	replacementBranch trueBranch: aDRBasicBlock endInstruction trueBranch.
	replacementBranch falseBranch: aDRBasicBlock endInstruction falseBranch.
	aDRBasicBlock endInstruction replaceBy: replacementBranch
]

{ #category : #testing }
DRBranchCollapse >> canBeAppliedIn: aDRControlFlowGraph [

	^ aDRControlFlowGraph blocks anySatisfy: [ :b | self canBeAppliedToBlock: b ]
]

{ #category : #testing }
DRBranchCollapse >> canBeAppliedToBlock: aDRBasicBlock [

	^ aDRBasicBlock endInstruction isConditionalBranch and: [
		  aDRBasicBlock endInstruction condition isEqualsThanComparison and: [
			  aDRBasicBlock endInstruction operand1 isSetConditionCode and: [
				  aDRBasicBlock endInstruction operand2 = true asDRValue ] ] ]
]
