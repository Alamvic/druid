"
Since in cogit we cannot express operations with all constant operands,
We need to split instructions in two.
E.g.,
	
	A := 1 + 2
	=>
	B := 1
	A := B + 2
"
Class {
	#name : #DRCogitJITConstantSplitting,
	#superclass : #DROptimisation,
	#category : #'Druid-Cogit'
}

{ #category : #accessing }
DRCogitJITConstantSplitting >> applyTo: cfg [

	| copy |
	cfg instructionsDo: [ :i | 
		(i operands size = 2 and: [ 
			 i operands allSatisfy: [ :e | 
				 e isConstant or: [ e isObjectReferenceValue ] ] ]) ifTrue: [
			"Split the instruction in a copy and the instruction itself."
			"We need to split Druid compile time constants and Cogit JIT compile time constants.
			
			JIT compile time object references such as nilObject cannot be folded by druid,
			we need to split them to be able to generate cogit code.
			
			We did not make object references druid compile time constants because that would impact algorithms that use constants such as SCCP"
			copy := cfg instructionFactory copy: i operand1.
			i replaceOperandAtIndex: 1 by: copy.
			i addBefore: copy ] ]
]
