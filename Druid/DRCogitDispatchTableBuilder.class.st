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

{ #category : #'accessing - building' }
DRCogitDispatchTableBuilder >> buildInitializePrimitiveTableAssignmentNode [
	"Answer a <PCGMessageNode> with the primitive table initialization setter message send"

	^ PCGMessageNode
			receiver: #self asPCGNode
			selector: #table:from:
			arguments: { self buildPrimitiveTableVariableNode . #'self primitiveTableArray' asPCGArgument }
]

{ #category : #'accessing - building' }
DRCogitDispatchTableBuilder >> buildInitializePrimitiveTableIVarAssignmentNode [

	^ PCGAssignmentNode new
		  variable: self buildPrimitiveTableVariableNode;
		  value: self buildPrimitiveTableInitialValueNode
]

{ #category : #'accessing - building' }
DRCogitDispatchTableBuilder >> buildInitializePrimitiveTableMethodNode [
	"Answer a <PCGMethodNode> ready for installation in a JIT compiler class"

	^ (PCGMethodNode selector: self initPrimitiveTableSelector) bodyBlock: [ : body |
		  body
			<< self buildInitializePrimitiveTableMaxCompiledPrimitiveIndexNode;
			<< self buildInitializePrimitiveTableIVarAssignmentNode;
			<< self buildInitializePrimitiveTableAssignmentNode;
			<< self buildPrimitiveTableVariableNode returnIt ]
]

{ #category : #accessing }
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

{ #category : #'accessing - building' }
DRCogitDispatchTableBuilder >> buildPrimitiveTableVariableNode [

	^ #primitiveTable asPCGInstanceVariable
]

{ #category : #'accessing - building' }
DRCogitDispatchTableBuilder >> initPrimitiveTableSelector [ 

	^ #initializePrimitiveTable
]

{ #category : #accessing }
DRCogitDispatchTableBuilder >> maxPrimitiveIndexGlobalName [

	^ #MaxCompiledPrimitiveIndex
]
