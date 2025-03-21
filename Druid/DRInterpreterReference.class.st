Class {
	#name : 'DRInterpreterReference',
	#superclass : 'DRValue',
	#instVars : [
		'reference',
		'type'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'accessing' }
DRInterpreterReference class >> argumentCount [

	^ self integerExpression: 'self methodNumArgs'
]

{ #category : 'accessing' }
DRInterpreterReference class >> classTableFirstPage [

	^ self objectReference: 'objectMemory hiddenRootsObject + 8'
]

{ #category : 'factory' }
DRInterpreterReference class >> falseObject [

	^ self pointerReference: 'objectMemory falseObject'
]

{ #category : 'factory' }
DRInterpreterReference class >> freeStartAddress [

	^ self pointerReference: 'objectMemory freeStartAddress'
]

{ #category : 'accesing - defaults' }
DRInterpreterReference class >> getFixedPermSpaceStart [

	^ self pointerReference: 'objectMemory getMemoryMap fixedPermSpaceStart'
]

{ #category : 'accessing' }
DRInterpreterReference class >> getNewSpaceMask [

	^ self pointerReference: 'objectMemory getMemoryMap getNewSpaceMask'
]

{ #category : 'accessing' }
DRInterpreterReference class >> getNewSpaceStart [

	^ self pointerReference: 'objectMemory getMemoryMap getNewSpaceStart'
]

{ #category : 'accessing' }
DRInterpreterReference class >> getOldSpaceMask [

	^ self pointerReference: 'objectMemory getMemoryMap getOldSpaceMask'
]

{ #category : 'accessing' }
DRInterpreterReference class >> getPermSpaceMask [

	^ self pointerReference: 'objectMemory getMemoryMap getPermSpaceMask'
]

{ #category : 'accessing' }
DRInterpreterReference class >> getSpaceMaskToUse [

	^ self pointerReference: 'objectMemory getMemoryMap getSpaceMaskToUse'
]

{ #category : 'factory' }
DRInterpreterReference class >> hiddenRootsObject [

	^ self pointerReference: 'objectMemory hiddenRootsObject'
]

{ #category : 'accessing' }
DRInterpreterReference class >> integerExpression: aReference [

	^ (self reference: aReference) type: (DRUnsignedIntegerType size: 8)
]

{ #category : 'accessing' }
DRInterpreterReference class >> methodObj [

	^ self objectReference: 'methodObj'
]

{ #category : 'factory' }
DRInterpreterReference class >> nilObject [

	^ self pointerReference: 'objectMemory nilObject'
]

{ #category : 'accessing' }
DRInterpreterReference class >> objectReference: aReference [

	^ (self reference: aReference) type: DRObjectReferenceType new
]

{ #category : 'accessing' }
DRInterpreterReference class >> pointerReference: aReference [

	^ (self reference: aReference) type: DRPointerType new
]

{ #category : 'factory' }
DRInterpreterReference class >> primitiveFailureCodeAddress [

	^ self pointerReference: 'coInterpreter primitiveFailureCodeAddress'
]

{ #category : 'accessing' }
DRInterpreterReference class >> reference: aReference [

	^ self new
		reference: aReference;
		yourself
]

{ #category : 'factory' }
DRInterpreterReference class >> trueObject [

	^ self pointerReference: 'objectMemory trueObject'
]

{ #category : 'visiting' }
DRInterpreterReference >> acceptVisitor: aVisitor [
	
	^ aVisitor visitInterpreterReference: self
]

{ #category : 'converting' }
DRInterpreterReference >> asBoolean [

	reference = self class trueObject reference ifTrue: [ ^ true ].
	reference = self class falseObject reference ifTrue: [ ^ false ].

	self error: 'InterpreterReference is not a boolean'
]

{ #category : 'converting' }
DRInterpreterReference >> asJitCompileTimeExpression [

	^ reference asDRValue asJitCompileTimeExpression
]

{ #category : 'testing' }
DRInterpreterReference >> initialize [

	super initialize.
	type := DRPointerType new
]

{ #category : 'testing' }
DRInterpreterReference >> isBooleanValue [

	^ {
		  self class trueObject reference.
		  self class falseObject reference } includes: reference
]

{ #category : 'testing' }
DRInterpreterReference >> isConstanteable [ 

	"Values known by the interpreter are always staged, and SHOULD be staged"
	^ self type isObjectReference not
]

{ #category : 'testing' }
DRInterpreterReference >> isInterpreterReference [

	^ true
]

{ #category : 'testing' }
DRInterpreterReference >> isJITCompileTimeExpression: aString [ 
	
	^ reference = aString
]

{ #category : 'accessing' }
DRInterpreterReference >> name [

	^ reference
]

{ #category : 'accessing' }
DRInterpreterReference >> operands [
	
	^ #()
]

{ #category : 'printing' }
DRInterpreterReference >> prettyPrint [

	^ reference
]

{ #category : 'accessing' }
DRInterpreterReference >> reference [

	^ reference
]

{ #category : 'accessing' }
DRInterpreterReference >> reference: aString [ 
	reference := aString
]

{ #category : 'cogit' }
DRInterpreterReference >> rtlArgumentExpressions: generator [

	^ {RBVariableNode named: self name}
]

{ #category : 'translating' }
DRInterpreterReference >> rtlArgumentsForAbsoluteAddressForGenerator: anObject [

	^ { (RBParser parseExpression: reference) }
]

{ #category : 'cogit-generation' }
DRInterpreterReference >> rtlOperandQualifier [
	
	^ 'Cq:'
]

{ #category : 'SCCP' }
DRInterpreterReference >> sccpEquals: other [

	^ other asDRValue isInterpreterReference and: [ reference = other reference ]
]

{ #category : 'SCCP' }
DRInterpreterReference >> sccpLatticeValueFor: aDRSCCP [ 

	^ aDRSCCP bottom 
]

{ #category : 'accessing' }
DRInterpreterReference >> type [

	^ type
]

{ #category : 'accessing' }
DRInterpreterReference >> type: anObject [

	type := anObject
]
