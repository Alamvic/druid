Class {
	#name : #DRStackInterpreterDispatchTableGenerator,
	#superclass : #DRAbstractDispatchTableGenerator,
	#instVars : [
		'primitiveSpec'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #'accessing - building' }
DRStackInterpreterDispatchTableGenerator >> arrayAccessorGlobalName [

	^ #Array
]

{ #category : #'accessing - building' }
DRStackInterpreterDispatchTableGenerator >> buildInitializePrimitiveTableAssignmentNode [
	"Answer a <PCGMessageNode> with the primitive table initialization setter message send"

	^ PCGMessageNode
			receiver: (PCGMessageNode
				receiver: #self asPCGNode
				selector: #primitivesClass)
			selector: #table:from:
			arguments: { self buildPrimitiveTableVariableNode . #'self primitiveTableArray' asPCGArgument }
]

{ #category : #accessing }
DRStackInterpreterDispatchTableGenerator >> buildPrimitiveTableInitialValueNode [

	^ PCGMessageNode
			receiver: #Array asPCGGlobal
			selector: #new:
			argument: (PCGMessageNode
				receiver: self maxPrimitiveIndexGlobalName asPCGGlobal
				selector: #+
				arguments: { 1 asPCG })
]

{ #category : #'accessing - building' }
DRStackInterpreterDispatchTableGenerator >> buildPrimitiveTableVariableNode [

	^ #PrimitiveTable asPCGGlobal
]

{ #category : #'accessing - building' }
DRStackInterpreterDispatchTableGenerator >> initPrimitiveTableSelector [ 

	^ #basicPrimitiveTable
]

{ #category : #accessing }
DRStackInterpreterDispatchTableGenerator >> maxPrimitiveIndexGlobalName [

	^ #MaxPrimitiveIndex
]

{ #category : #accessing }
DRStackInterpreterDispatchTableGenerator >> primitiveSpec [

	^ primitiveSpec
]

{ #category : #accessing }
DRStackInterpreterDispatchTableGenerator >> primitiveSpec: anObject [

	primitiveSpec := anObject
]

{ #category : #initialization }
DRStackInterpreterDispatchTableGenerator >> primitiveTable [
	"Answer a <Collection> with primitive table entries"

	^ self primitiveSpec
]
