"
Uses a Pharo Code Generator to build methods conforming to the Cogit JIT dispatch table specification. Currently these can be found browsing :

- #primitiveTable
- #initializePrimitiveTable

"
Class {
	#name : 'DRCogitDispatchTableGenerator',
	#superclass : 'DRAbstractDispatchTableGenerator',
	#category : 'Druid-CompilerBuilder',
	#package : 'Druid',
	#tag : 'CompilerBuilder'
}

{ #category : 'accessing - building' }
DRCogitDispatchTableGenerator >> arrayAccessorGlobalName [

	^ #CArrayAccessor
]

{ #category : 'as yet unclassified' }
DRCogitDispatchTableGenerator >> buildBytecodeTableMethodNode [
	"Sort by bytecode number"

	| sortedBytecodeTable |
	sortedBytecodeTable := self bytecodeTable sorted: [ :e1 :e2 |
		                       e1 second < e2 second ].
	^ RBMethodNode
		  selector: #bytecodeTable
		  body: (RBReturnNode value: sortedBytecodeTable asArray asLiteralNode) asSequenceNode
]

{ #category : 'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> buildInitializeBytecodeTableAssignmentNode [

	^ RBMessageNode
		  receiver: RBVariableNode selfNode
		  selector: #generatorTableFrom:
		  arguments: { (RBMessageNode
				   receiver: RBVariableNode selfNode
				   selector: self bytecodeTableVariableName) } 
]

{ #category : 'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> buildInitializeBytecodeTableBytecodeSetHasDirectedSuperSendNode [

	^ RBAssignmentNode
		  variable: (RBVariableNode named: #BytecodeSetHasDirectedSuperSend)
		  value: true asLiteralNode 
]

{ #category : 'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> buildInitializeBytecodeTableBytecodeSetHasExtensionsNode [

	^ RBAssignmentNode
		  variable: (RBVariableNode named: #BytecodeSetHasExtensions)
		  value: true asLiteralNode
]

{ #category : 'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> buildInitializeBytecodeTableFirstSpecialSelectorNode [

	^ RBAssignmentNode
		  variable: (RBVariableNode named: #FirstSpecialSelector)
		  value: 96 asLiteralNode
]

{ #category : 'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> buildInitializeBytecodeTableMethodNode [
	"Answer a <PCGMethodNode> ready for installation in a JIT compiler class"

	^ RBMethodNode
		  selector: self initBytecodeTableSelector
		  body: (RBSequenceNode statements: {
					   self buildInitializeBytecodeTablenumPushNilsFunctionNode.
					   self buildInitializeBytecodeTablepushNilSizeFunctionNode.
					   self buildInitializeBytecodeTableBytecodeSetHasDirectedSuperSendNode.
					   self buildInitializeBytecodeTableBytecodeSetHasExtensionsNode.
					   self buildInitializeBytecodeTableFirstSpecialSelectorNode.
					   self buildInitializeBytecodeTableNumSpecialSelectorsNode.
					   self buildInitializeBytecodeTableAssignmentNode })
]

{ #category : 'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> buildInitializeBytecodeTableNumSpecialSelectorsNode [

	^ RBAssignmentNode
		  variable: (RBVariableNode named: #NumSpecialSelectors)
		  value: 32 asLiteralNode 
]

{ #category : 'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> buildInitializeBytecodeTablenumPushNilsFunctionNode [

	compilationUnit targetClass isSimpleStack ifTrue: [
		^ nil asLiteralNode ].

	^ RBAssignmentNode
		  variable: (RBVariableNode named: #numPushNilsFunction)
		  value: #sistaV1:Num:Push:Nils: asLiteralNode
]

{ #category : 'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> buildInitializeBytecodeTablepushNilSizeFunctionNode [

	compilationUnit targetClass isSimpleStack ifTrue: [
		^ nil asLiteralNode ].

	^ RBAssignmentNode
		  variable: (RBVariableNode named: #pushNilSizeFunction)
		  value: #sistaV1PushNilSize:numInitialNils: asLiteralNode
]

{ #category : 'accessing - primitives' }
DRCogitDispatchTableGenerator >> buildInitializePrimitiveTableAssignmentNode [
	"Answer a <PCGMessageNode> with the primitive table initialization setter message send"

	^ RBMessageNode
		  receiver: RBVariableNode selfNode
		  selector: #table:from:
		  arguments: {
				  (RBVariableNode named: self primitiveTableVariableName).
				  (RBMessageNode
					   receiver: RBVariableNode selfNode
					   selector: #primitiveTableArray) }
]

{ #category : 'accessing - primitives' }
DRCogitDispatchTableGenerator >> buildPrimitiveTableInitialValueNode [

	^ RBMessageNode
		  receiver: (RBVariableNode named: self arrayAccessorGlobalName)
		  selector: #on:
		  arguments: { (RBMessageNode
				   receiver: (RBVariableNode named: #Array)
				   selector: #new:
				   arguments: { (RBMessageNode
						    receiver:
						    (RBVariableNode named: self maxPrimitiveIndexGlobalName)
						    selector: #+
						    arguments: { 1 asLiteralNode }) }) }
]

{ #category : 'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> bytecodeTable [
	"Answer a <Collection> of bytecode table entries"
	
	^ self compilationUnit bytecodeTable 
]

{ #category : 'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> bytecodeTableVariableName [

	^ #bytecodeTable
]

{ #category : 'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> initBytecodeTableSelector [ 

	^ #initializeBytecodeTableForSistaV1
]

{ #category : 'accessing - primitives' }
DRCogitDispatchTableGenerator >> initPrimitiveTableSelector [ 

	^ #initializePrimitiveTable
]

{ #category : 'accessing - installation' }
DRCogitDispatchTableGenerator >> installAllMethodsOn: aJITCompilerClass [
	"Answer a <PCGMethodNode> ready for installation in a JIT compiler class"

	^ {
		  self buildInitializePrimitiveTableArrayMethodNode.
		  self buildInitializePrimitiveTableMethodNode.
		  self buildBytecodeTableMethodNode.
		  self buildInitializeBytecodeTableMethodNode }
		  do: [ :methodNode |
			  self addGeneratedComment: methodNode.
			  aJITCompilerClass
				  compile: methodNode formattedCode
				  classified: #'class initialization'
				  notifying: nil ]
		  displayingProgress:
		  'Installing methods on ' , aJITCompilerClass name
]

{ #category : 'accessing - primitives' }
DRCogitDispatchTableGenerator >> maxPrimitiveIndexGlobalName [

	^ #MaxCompiledPrimitiveIndex
]
