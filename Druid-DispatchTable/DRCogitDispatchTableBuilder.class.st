Class {
	#name : #DRCogitDispatchTableBuilder,
	#superclass : #DRAbstractDispatchTableBuilder,
	#category : #'Druid-DispatchTable-Model'
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

{ #category : #accessing }
DRCogitDispatchTableBuilder >> buildPrimitiveTableInitialValueNode [

	^ PCGMessageNode
		receiver: self arrayAccessorGlobalName asPCGGlobal
		selector: #on:
		arguments:
			(PCGMessageNode
				receiver: #Array asPCGGlobal
				selector: #new:
				arguments: (PCGMessageNode
					receiver: self maxPrimitiveIndexGlobalName asPCGGlobal
					selector: #+
					arguments: { 1 asPCG }) )
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
