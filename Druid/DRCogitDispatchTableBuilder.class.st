"
Uses a Pharo Code Generator to build methods conforming to the Cogit JIT dispatch table specification. Currently these can be found browsing :

- #primitiveTable
- #initializePrimitiveTable

"
Class {
	#name : #DRCogitDispatchTableBuilder,
	#superclass : #DRAbstractDispatchTableBuilder,
	#category : #'Druid-CompilerBuilder'
}

{ #category : #'accessing - building' }
DRCogitDispatchTableBuilder >> arrayAccessorGlobalName [

	^ #CArrayAccessor
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableBuilder >> buildBytecodeTableVariableNode [
	"Answer a <PCGReturnNode> with the primitive table initialization setter message send"

	^ self initBytecodeTable asPCG returnIt
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableBuilder >> buildInitializeBytecodeTableAssignmentNode [
	"Answer a <PCGMessageNode> with the bytecode table initialization setter message send"

	^ PCGMessageNode
			receiver: #self asPCGNode
			selector: #generatorTableFrom:
			arguments: { self buildBytecodeTableVariableNode asPCGArgument }
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableBuilder >> buildInitializeBytecodeTableBytecodeSetHasDirectedSuperSendNode [

	^ PCGAssignmentNode new
		  variable: #BytecodeSetHasDirectedSuperSend asPCGGlobal;
		  value: true asPCG
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableBuilder >> buildInitializeBytecodeTableBytecodeSetHasExtensionsNode [

	^ PCGAssignmentNode new
		  variable: #BytecodeSetHasExtensions asPCGGlobal;
		  value: true asPCG
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableBuilder >> buildInitializeBytecodeTableFirstSpecialSelectorNode [

	^ PCGAssignmentNode new
		  variable: #FirstSpecialSelector asPCGGlobal;
		  value: 96 asPCG
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableBuilder >> buildInitializeBytecodeTableMethodNode [
	"Answer a <PCGMethodNode> ready for installation in a JIT compiler class"

	^ (PCGMethodNode selector: self initBytecodeTableSelector) bodyBlock: [ : body |
		  body
			<< self buildInitializeBytecodeTablenumPushNilsFunctionNode;
			<< self buildInitializeBytecodeTablepushNilSizeFunctionNode;
			<< self buildInitializeBytecodeTableBytecodeSetHasDirectedSuperSendNode;
			<< self buildInitializeBytecodeTableBytecodeSetHasExtensionsNode;
			<< self buildInitializeBytecodeTableFirstSpecialSelectorNode;
			<< self buildInitializeBytecodeTableNumSpecialSelectorsNode;
			<< self buildInitializeBytecodeTableAssignmentNode returnIt ]
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableBuilder >> buildInitializeBytecodeTableNumSpecialSelectorsNode [

	^ PCGAssignmentNode new
		  variable: #NumSpecialSelectors asPCGGlobal;
		  value: 32 asPCG
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableBuilder >> buildInitializeBytecodeTablenumPushNilsFunctionNode [

	^ PCGAssignmentNode new
		  variable: #numPushNilsFunction asPCG;
		  value: #sistaV1:Num:Push:Nils: asPCG
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableBuilder >> buildInitializeBytecodeTablepushNilSizeFunctionNode [

	^ PCGAssignmentNode new
		  variable: #pushNilSizeFunction asPCG;
		  value: #sistaV1PushNilSize:numInitialNils: asPCG
]

{ #category : #'accessing - primitives' }
DRCogitDispatchTableBuilder >> buildInitializePrimitiveTableAssignmentNode [
	"Answer a <PCGMessageNode> with the primitive table initialization setter message send"

	^ PCGMessageNode
			receiver: #self asPCGNode
			selector: #table:from:
			arguments: { self buildPrimitiveTableVariableNode . #'self primitiveTableArray' asPCGArgument }
]

{ #category : #'accessing - primitives' }
DRCogitDispatchTableBuilder >> buildPrimitiveTableInitialValueNode [

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
DRCogitDispatchTableBuilder >> buildPrimitiveTableVariableNode [

	^ #primitiveTable asPCGInstanceVariable
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableBuilder >> initBytecodeTable [
	"Answer a <Collection> of bytecode table entries"
	
	^ self compilationUnit initBytecodeTable 
]

{ #category : #'accessing - bytecodes' }
DRCogitDispatchTableBuilder >> initBytecodeTableSelector [ 

	^ #initializeBytecodeTableForSistaV1
]

{ #category : #'accessing - primitives' }
DRCogitDispatchTableBuilder >> initPrimitiveTableSelector [ 

	^ #initializePrimitiveTable
]

{ #category : #'accessing - primitives' }
DRCogitDispatchTableBuilder >> maxPrimitiveIndexGlobalName [

	^ #MaxCompiledPrimitiveIndex
]
