Class {
	#name : #DRStackInterpreterDispatchTableBuilder,
	#superclass : #DRAbstractDispatchTableBuilder,
	#category : #'Druid-DispatchTable'
}

{ #category : #'accessing - building' }
DRStackInterpreterDispatchTableBuilder >> arrayAccessorGlobalName [

	^ #Array
]

{ #category : #'accessing - building' }
DRStackInterpreterDispatchTableBuilder >> buildInitializePrimitiveTableAssignmentNode [
	"Answer a <PCGMessageNode> with the primitive table initialization setter message send"

	^ PCGMessageNode
			receiver: (PCGMessageNode
				receiver: #self asPCGNode
				selector: #primitivesClass)
			selector: #table:from:
			arguments: { self buildPrimitiveTableVariableNode . #'self primitiveTableArray' asPCGArgument }
]

{ #category : #accessing }
DRStackInterpreterDispatchTableBuilder >> buildPrimitiveTableInitialValueNode [

	^ PCGMessageNode
			receiver: #Array asPCGGlobal
			selector: #new:
			argument: (PCGMessageNode
				receiver: self maxPrimitiveIndexGlobalName asPCGGlobal
				selector: #+
				arguments: { 1 asPCG })
]

{ #category : #'accessing - building' }
DRStackInterpreterDispatchTableBuilder >> buildPrimitiveTableVariableNode [

	^ #PrimitiveTable asPCGGlobal
]

{ #category : #initialization }
DRStackInterpreterDispatchTableBuilder >> initPrimitiveTable [
	"Answer a <Collection> with primitive table entries"

	^ self primitiveSpec
]

{ #category : #'accessing - building' }
DRStackInterpreterDispatchTableBuilder >> initPrimitiveTableSelector [ 

	^ #basicPrimitiveTable
]

{ #category : #accessing }
DRStackInterpreterDispatchTableBuilder >> maxPrimitiveIndexGlobalName [

	^ #MaxPrimitiveIndex
]
