Class {
	#name : #DRInterpreterReference,
	#superclass : #DRValue,
	#instVars : [
		'reference',
		'type'
	],
	#category : #'Druid-IR'
}

{ #category : #factory }
DRInterpreterReference class >> falseObject [

	^ self objectReference: 'objectMemory falseObject'
]

{ #category : #factory }
DRInterpreterReference class >> freeStartAddress [

	^ self pointerReference: 'objectMemory freeStartAddress'
]

{ #category : #accessing }
DRInterpreterReference class >> getNewSpaceMask [

	^ self pointerReference: 'objectMemory getMemoryMap getNewSpaceMask'
]

{ #category : #accessing }
DRInterpreterReference class >> getNewSpaceStart [

	^ self pointerReference: 'objectMemory getMemoryMap getNewSpaceStart'
]

{ #category : #accessing }
DRInterpreterReference class >> getOldSpaceMask [

	^ self pointerReference: 'objectMemory getMemoryMap getOldSpaceMask'
]

{ #category : #accessing }
DRInterpreterReference class >> getPermSpaceMask [

	^ self pointerReference: 'objectMemory getMemoryMap getPermSpaceMask'
]

{ #category : #accessing }
DRInterpreterReference class >> getSpaceMaskToUse [

	^ self pointerReference: 'objectMemory getMemoryMap getSpaceMaskToUse'
]

{ #category : #factory }
DRInterpreterReference class >> hiddenRootsObject [

	^ self pointerReference: 'objectMemory hiddenRootsObject'
]

{ #category : #factory }
DRInterpreterReference class >> nilObject [

	^ self objectReference: 'objectMemory nilObject'
]

{ #category : #accessing }
DRInterpreterReference class >> objectReference: aReference [

	^ (self reference: aReference) type: DRObjectReferenceType new
]

{ #category : #accessing }
DRInterpreterReference class >> pointerReference: aReference [

	^ (self reference: aReference) type: DRPointerType new
]

{ #category : #factory }
DRInterpreterReference class >> primitiveFailureCodeAddress [

	^ self pointerReference: 'coInterpreter primitiveFailureCodeAddress'
]

{ #category : #accessing }
DRInterpreterReference class >> reference: aReference [

	^ self new
		reference: aReference;
		yourself
]

{ #category : #factory }
DRInterpreterReference class >> trueObject [

	^ self objectReference: 'objectMemory trueObject'
]

{ #category : #visiting }
DRInterpreterReference >> acceptVisitor: aVisitor [
	
	^ aVisitor visitInterpreterReference: self
]

{ #category : #testing }
DRInterpreterReference >> initialize [

	super initialize.
	type := DRPointerType new
]

{ #category : #testing }
DRInterpreterReference >> isConstanteable [ 

	"Values known by the interpreter are always staged, and SHOULD be staged"
	^ self type isObjectReference not
]

{ #category : #testing }
DRInterpreterReference >> isInterpreterReference [

	^ true
]

{ #category : #testing }
DRInterpreterReference >> isJITCompileTimeExpression: aString [ 
	
	^ reference = aString
]

{ #category : #testing }
DRInterpreterReference >> isStaged [

	"Values known by the interpreter are always staged, and SHOULD be staged"
	^ self type isObjectReference not
]

{ #category : #accessing }
DRInterpreterReference >> name [

	^ reference
]

{ #category : #accessing }
DRInterpreterReference >> operands [
	
	^ #()
]

{ #category : #printing }
DRInterpreterReference >> prettyPrint [

	^ reference
]

{ #category : #accessing }
DRInterpreterReference >> reference [

	^ reference
]

{ #category : #accessing }
DRInterpreterReference >> reference: aString [ 
	reference := aString
]

{ #category : #cogit }
DRInterpreterReference >> rtlArgumentExpressions: generator [

	^ {RBVariableNode named: self name}
]

{ #category : #translating }
DRInterpreterReference >> rtlArgumentsForAbsoluteAddressForGenerator: anObject [

	^ { (RBParser parseExpression: reference) }
]

{ #category : #'cogit-generation' }
DRInterpreterReference >> rtlOperandQualifier [
	
	^ 'Cq:'
]

{ #category : #SCCP }
DRInterpreterReference >> sccpLatticeValueFor: aDRSCCP [ 

	^ aDRSCCP bottom 
]

{ #category : #accessing }
DRInterpreterReference >> type [

	^ type
]

{ #category : #accessing }
DRInterpreterReference >> type: anObject [

	type := anObject
]
