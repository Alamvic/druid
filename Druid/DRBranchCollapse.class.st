Class {
	#name : #DRBranchCollapse,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRBranchCollapse >> applyTo: aDRControlFlowGraph [

	"If we have a sequence of the form: 
	
	R1 := SetConditionCode a > b
	Jump if R1 = true
	
	=>
	
	Jump if a > b
	"

	aDRControlFlowGraph blocksDo: [ :b |
		(b endInstruction condition isEqualsThanComparison and: [
			 b endInstruction operand1 isSetConditionCode and: [
				 b endInstruction operand2 = true asDRValue ] ]) ifTrue: [
			| replacementBranch setConditionCode |
			setConditionCode := b endInstruction operand1 simpleConstantFold.
			replacementBranch := DRBranchIfCondition operands: {
					                     setConditionCode condition.
					                     setConditionCode operand1.
					                     setConditionCode operand2 }.
			replacementBranch trueBranch: b endInstruction trueBranch.
			replacementBranch falseBranch: b endInstruction falseBranch.
			b endInstruction replaceBy: replacementBranch ] ]
]

{ #category : #testing }
DRBranchCollapse >> canBeAppliedIn: aDRControlFlowGraph [

	aDRControlFlowGraph blocksDo: [ :b | 
		(b endInstruction condition isEqualsThanComparison and: [ 
			b endInstruction operand1 isSetConditionCode
				and: [ b endInstruction operand2 = true asDRValue ] ]) ifTrue: [ 
			^ true ] ].
	^ false
]
