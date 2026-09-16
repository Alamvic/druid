Class {
	#name : 'DRBlockClosure',
	#superclass : 'DRCode',
	#instVars : [
		'outerContext',
		'blockNode',
		'irGenerator',
		'blockCFG'
	],
	#category : 'Druid-CompilerCompiler',
	#package : 'Druid',
	#tag : 'CompilerCompiler'
}

{ #category : 'comparing' }
DRBlockClosure >> = otherCode [

	self class = otherCode class ifTrue: [ ^ self blockNode == otherCode blockNode ].
	^ false
]

{ #category : 'visiting' }
DRBlockClosure >> acceptVisitor: aVisitor [

	^ aVisitor visitBlockClosure: self
]

{ #category : 'accessing' }
DRBlockClosure >> argName: index [

	^ blockNode argumentNames at: index
]

{ #category : 'activating' }
DRBlockClosure >> basicNewFrame [

	^ DRClosureStackFrame new
]

{ #category : 'accessing' }
DRBlockClosure >> blockCFG [
	^ blockCFG
]

{ #category : 'as yet unclassified' }
DRBlockClosure >> blockCFG: aDRControlFlowGraph [

	blockCFG := aDRControlFlowGraph
]

{ #category : 'accessing' }
DRBlockClosure >> blockNode [

	^ blockNode
]

{ #category : 'accessing' }
DRBlockClosure >> blockNode: anObject [

	blockNode := anObject
]

{ #category : 'accessing' }
DRBlockClosure >> codeNode [
	
	^ blockNode
]

{ #category : 'activation' }
DRBlockClosure >> defaultReturnValue: receiver [

	^ DRNullValue new
]

{ #category : 'executing' }
DRBlockClosure >> executeOn: interpreter [
	
	interpreter interpretBlockActivation: blockNode
]

{ #category : 'comparing' }
DRBlockClosure >> hash [

	^ blockNode hash
]

{ #category : 'accessing' }
DRBlockClosure >> irGenerator [
1halt.
	^ irGenerator
]

{ #category : 'accessing' }
DRBlockClosure >> irGenerator: anIRGenerator [
1halt.
	irGenerator := anIRGenerator
]

{ #category : 'testing' }
DRBlockClosure >> isDRBlockClosure [
	
	^ true
]

{ #category : 'testing' }
DRBlockClosure >> isInstruction [

	^ false
]

{ #category : 'ir-generation' }
DRBlockClosure >> methodTopFrame [

	^ outerContext deferredMethodReturns values unique topFrame
]

{ #category : 'accessing' }
DRBlockClosure >> name [

	^ 'a block'
]

{ #category : 'accessing' }
DRBlockClosure >> numberOfArguments [
	
	^ blockNode arguments size
]

{ #category : 'printing' }
DRBlockClosure >> opcode [
	
	^ 'DRBlock ', blockNode asString 
]

{ #category : 'inspecting' }
DRBlockClosure >> originAST [

	^ blockNode
]

{ #category : 'accessing' }
DRBlockClosure >> outerContext [

	^ outerContext
]

{ #category : 'accessing' }
DRBlockClosure >> outerContext: anObject [

	outerContext := anObject
]

{ #category : 'SCCP' }
DRBlockClosure >> sccpLatticeValueFor: sccp [ 
	
	"This instruction has no value"
	^ sccp bottom
]

{ #category : 'activation' }
DRBlockClosure >> typeWithAlreadySeen: aCollection [

	^ BlockClosure asDRType 
]
