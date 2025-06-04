Class {
	#name : 'DRBlockIRGenerator',
	#superclass : 'DRMethodIRGenerator',
	#category : 'Druid-CompilerCompiler',
	#package : 'Druid',
	#tag : 'CompilerCompiler'
}

{ #category : 'visiting' }
DRBlockIRGenerator >> interpretBlockActivation: aRBBlockNode [

	| result copy |
	self interpretBlockBody: aRBBlockNode.
	self ir lastBasicBlock endInstruction isReturn ifTrue: [ ^ self ].
	result := self operandStackTop.
	copy := self instructionFor: result inNode: aRBBlockNode.
	self topFrame returnValue: copy.
	currentBasicBlock blockReturn: copy
]

{ #category : 'visiting' }
DRBlockIRGenerator >> visitReturnNode: aRBReturnNode [

	| value |
	
	value := self visitOperand: aRBReturnNode value.

	self pushOperand: value.
	currentBasicBlock return: value.
]
