Class {
	#name : #DRCommutativeOperation,
	#superclass : #DRArithmeticInstruction,
	#category : #'Druid-IR'
}

{ #category : #converting }
DRCommutativeOperation >> reversed [

	^ self class operands: operands reversed result: result
]

{ #category : #'constant folding' }
DRCommutativeOperation >> simpleConstantFold [

	| operand1 operand2 |
	operand1 := self operand1 simpleConstantFold.
	operand2 := self operand2 simpleConstantFold.

	operand1 isNumber ifFalse: [ ^ self ].
	operand2 isNumber ifFalse: [ ^ self ].

	^ self sccpOperateOnLattice: operand1 and: operand2
]
