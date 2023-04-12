"
Uses a Pharo Code Generator to build methods conforming to the Cogit JIT dispatch table specification. Currently these can be found browsing :

- #primitiveTable
- #initializePrimitiveTable

"
Class {
	#name : #DRCogitDispatchTableGenerator,
	#superclass : #DRAbstractDispatchTableGenerator,
	#category : #'Druid-CompilerBuilder'
}

{ #category : #'accessing - building' }
DRCogitDispatchTableGenerator >> arrayAccessorGlobalName [

	^ #CArrayAccessor
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> buildBytecodeTableArrayNode [

	^ self bytecodeTable asPCG returnIt
]

{ #category : #'as yet unclassified' }
DRCogitDispatchTableGenerator >> buildBytecodeTableMethodNode [

	^ (PCGMethodNode selector: #bytecodeTable) bodyBlock: [ :body |
		  body << self bytecodeTable asPCG returnIt ]
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> buildInitializeBytecodeTableAssignmentNode [
	"Answer a <PCGMessageNode> with the bytecode table initialization setter message send"

	^ PCGMessageNode
			receiver: #self asPCGNode
			selector: #generatorTableFrom:
			arguments: { #'self bytecodeTable' asPCGArgument  }
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> buildInitializeBytecodeTableBytecodeSetHasDirectedSuperSendNode [

	^ PCGAssignmentNode new
		  variable: #BytecodeSetHasDirectedSuperSend asPCGGlobal;
		  value: true asPCG
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> buildInitializeBytecodeTableBytecodeSetHasExtensionsNode [

	^ PCGAssignmentNode new
		  variable: #BytecodeSetHasExtensions asPCGGlobal;
		  value: true asPCG
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> buildInitializeBytecodeTableFirstSpecialSelectorNode [

	^ PCGAssignmentNode new
		  variable: #FirstSpecialSelector asPCGGlobal;
		  value: 96 asPCG
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> buildInitializeBytecodeTableMethodNode [
	"Answer a <PCGMethodNode> ready for installation in a JIT compiler class"

	^ (PCGMethodNode selector: self initBytecodeTableSelector) bodyBlock: [ : body |
		  body
			<< self buildInitializeBytecodeTablenumPushNilsFunctionNode;
			<< self buildInitializeBytecodeTablepushNilSizeFunctionNode;
			<< self buildInitializeBytecodeTableBytecodeSetHasDirectedSuperSendNode;
			<< self buildInitializeBytecodeTableBytecodeSetHasExtensionsNode;
			<< self buildInitializeBytecodeTableFirstSpecialSelectorNode;
			<< self buildInitializeBytecodeTableNumSpecialSelectorsNode;
			<< self buildInitializeBytecodeTableAssignmentNode ]
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> buildInitializeBytecodeTableNumSpecialSelectorsNode [

	^ PCGAssignmentNode new
		  variable: #NumSpecialSelectors asPCGGlobal;
		  value: 32 asPCG
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> buildInitializeBytecodeTablenumPushNilsFunctionNode [

	^ PCGAssignmentNode new
		  variable: #numPushNilsFunction asPCGInstanceVariable;
		  value: #sistaV1:Num:Push:Nils: asPCG
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> buildInitializeBytecodeTablepushNilSizeFunctionNode [

	^ PCGAssignmentNode new
		  variable: #pushNilSizeFunction asPCGInstanceVariable;
		  value: #sistaV1PushNilSize:numInitialNils: asPCG
]

{ #category : #'accessing - primitives' }
DRCogitDispatchTableGenerator >> buildInitializePrimitiveTableAssignmentNode [
	"Answer a <PCGMessageNode> with the primitive table initialization setter message send"

	^ PCGMessageNode
			receiver: #self asPCGNode
			selector: #table:from:
			arguments: { self buildPrimitiveTableVariableNode . #'self primitiveTableArray' asPCGArgument }
]

{ #category : #'accessing - primitives' }
DRCogitDispatchTableGenerator >> buildPrimitiveTableInitialValueNode [

	^ PCGMessageNode
		receiver: self arrayAccessorGlobalName asPCGGlobal
		selector: #on:
		argument:
			(PCGMessageNode
				receiver: #Array asPCGGlobal
				selector: #new:
				argument: (PCGMessageNode
					receiver: self maxPrimitiveIndexGlobalName asPCGGlobal
					selector: #+
					arguments: { 1 asPCG }))
]

{ #category : #'accessing - primitives' }
DRCogitDispatchTableGenerator >> buildPrimitiveTableVariableNode [

	^ #primitiveTable asPCGInstanceVariable
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> bytecodeTable [
	"Answer a <Collection> of bytecode table entries"
	
	^ self compilationUnit bytecodeTable 
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableGenerator >> initBytecodeTableSelector [ 

	^ #initializeBytecodeTableForSistaV1
]

{ #category : #'accessing - primitives' }
DRCogitDispatchTableGenerator >> initPrimitiveTableSelector [ 

	^ #initializePrimitiveTable
]

{ #category : #'accessing - installation' }
DRCogitDispatchTableGenerator >> installAllMethodsOn: aJITCompilerClass [
	"Answer a <PCGMethodNode> ready for installation in a JIT compiler class"

	^ {
		self buildInitializePrimitiveTableArrayMethodNode .
		self buildInitializePrimitiveTableMethodNode .
		self buildBytecodeTableMethodNode .
		self buildInitializeBytecodeTableMethodNode
		} do: [ : pcgNode |
			pcgNode
				protocol: #'class initialization';
				installOn: aJITCompilerClass ]
		displayingProgress: 'Installing methods'
]

{ #category : #'accessing - primitives' }
DRCogitDispatchTableGenerator >> maxPrimitiveIndexGlobalName [

	^ #MaxCompiledPrimitiveIndex
]
