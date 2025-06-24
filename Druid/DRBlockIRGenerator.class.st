Class {
	#name : 'DRBlockIRGenerator',
	#superclass : 'DRMethodIRGenerator',
	#category : 'Druid-CompilerCompiler',
	#package : 'Druid',
	#tag : 'CompilerCompiler'
}

{ #category : 'visiting' }
DRBlockIRGenerator >> finishCodeInterpretation: lastFrame [
	
	lastFrame returnedValue isReturn ifFalse: [ 
		currentBasicBlock blockReturn: lastFrame returnedValue.
	]
	
]

{ #category : 'visiting' }
DRBlockIRGenerator >> interpretBlockActivation: aRBBlockNode [

	| result copy |
	
	self interpretBlockBody: aRBBlockNode.
	
	result := self operandStackTop.
	copy := self instructionFor: result inNode: aRBBlockNode.
	self topFrame returnValue: copy.
	
]

{ #category : 'as yet unclassified' }
DRBlockIRGenerator >> temporariesListfrom: aNode [

	^ self scope allTempsWithOuterTemps 
]

{ #category : 'visiting' }
DRBlockIRGenerator >> visitReturnNode: aRBReturnNode [

	| value |
	
	value := self visitOperand: aRBReturnNode value.

	self pushOperand:(currentBasicBlock return: value).

]

{ #category : 'visiting' }
DRBlockIRGenerator >> visitSelfNode: aRBVariableNode [ 

	self pushOperand: self receiver.

]
