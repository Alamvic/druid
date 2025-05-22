Class {
	#name : 'DRBlockIRGenerator',
	#superclass : 'DRMethodIRGenerator',
	#category : 'Druid-CompilerCompiler',
	#package : 'Druid',
	#tag : 'CompilerCompiler'
}

{ #category : 'visiting' }
DRBlockIRGenerator >> finishCodeInterpretation: anInstruction [

	currentBasicBlock blockReturn: anInstruction 
]

{ #category : 'visiting' }
DRBlockIRGenerator >> interpretBlockActivation: aRBBlockNode [

	| result copy |
	self interpretBlockBody: aRBBlockNode.

	"add block return if no return in the block"
	self ir lastBasicBlock endInstruction isReturn ifFalse: [
		result := self operandStackTop.
		copy := self instructionFor: result inNode: aRBBlockNode.

		self topFrame returnValue: copy.
		self finishCodeInterpretation: copy ]
]

{ #category : 'visiting' }
DRBlockIRGenerator >> visitReturnNode: aRBReturnNode [

	| value |
	
	value := self visitOperand: aRBReturnNode value.

	self pushOperand: value.
	currentBasicBlock return: value.
]
