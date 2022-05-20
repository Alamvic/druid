Class {
	#name : #DRBlockClosure,
	#superclass : #DRValue,
	#instVars : [
		'outerContext',
		'blockNode'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRBlockClosure >> blockNode [

	^ blockNode
]

{ #category : #accessing }
DRBlockClosure >> blockNode: anObject [

	blockNode := anObject
]

{ #category : #accessing }
DRBlockClosure >> codeNode [
	
	^ blockNode
]

{ #category : #executing }
DRBlockClosure >> executeOn: interpreter [
	
	interpreter interpretBlockActivation: blockNode
]

{ #category : #accessing }
DRBlockClosure >> outerContext [

	^ outerContext
]

{ #category : #accessing }
DRBlockClosure >> outerContext: anObject [

	outerContext := anObject
]

{ #category : #activating }
DRBlockClosure >> pushFrameIn: interpreter receiver: aReceiver [ 
	| frame newMethodEntry incomingBasicBlock |	

	incomingBasicBlock := interpreter currentBasicBlock.
	newMethodEntry := interpreter newBasicBlock.
	incomingBasicBlock jumpTo: newMethodEntry.
	
	frame := DruidClosureStackFrame new
		receiver: aReceiver;
		method: self codeNode;
		irGenerator: interpreter;
		entryBasicBlock: newMethodEntry;
		exitBasicBlock: interpreter basicNewBasicBlock;
		returnValue: (DRPhiFunction new
			result: interpreter allocateTemporaryRegister;
			origin: thisContext stack;
			yourself);
		yourself.

	newMethodEntry executionState temporaryFrame: Dictionary new.
	newMethodEntry executionState operandStack: Stack new.
	interpreter pushFrame: frame
]
