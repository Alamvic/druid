Class {
	#name : #DRAbstractCompilerBuilder,
	#superclass : #Object,
	#traits : 'TDRMethodSelector',
	#classTraits : 'TDRMethodSelector classTrait',
	#instVars : [
		'compilationUnit'
	],
	#category : #'Druid-InterpreterBuilder'
}

{ #category : #accessing }
DRAbstractCompilerBuilder >> addPrimitive: aString [

	(self isValidPrimitiveName: aString)
		ifTrue: [ self compilationUnit addPrimitive: (self newPrimitive: aString) ]
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> addPrimitives [
	"Iterate over the receiver's interpreter (assumed to contain primitive methods) and add them to compilationUnit"

	| primitiveSelectors |
	primitiveSelectors := (self interpreterPrimitiveTable collect: [ : symOrCompiledMethod | 
		symOrCompiledMethod isCompiledMethod 
			ifTrue: [ symOrCompiledMethod selector ]
			ifFalse: [ symOrCompiledMethod ] ]).
	primitiveSelectors
		do: [ : prim | self addPrimitive: prim ]
		displayingProgress: [ : prim | 'Adding primitive: ' , prim asString ]
]

{ #category : #'accessing - compiler' }
DRAbstractCompilerBuilder >> compilationUnit [

	^ compilationUnit
		ifNil: [ 	compilationUnit := self compilationUnitClass new ]
]

{ #category : #'accessing - compiler' }
DRAbstractCompilerBuilder >> compilationUnit: anObject [

	compilationUnit := anObject
]

{ #category : #'accessing - compiler' }
DRAbstractCompilerBuilder >> compilationUnitClass [

	self subclassResponsibility
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> defaultTargetSuperclass [

	self subclassResponsibility

]

{ #category : #accessing }
DRAbstractCompilerBuilder >> environmentAt: aClass [
	"Answer the class named aClass. Create if it doesn't exist"

	^ self class environment
		at: aClass
		ifAbsent: [ self newClassNamed: aClass superclass: (Smalltalk at: self targetSuperclass) ]
]

{ #category : #DELETE }
DRAbstractCompilerBuilder >> generateBuildModel [
	"Answer a <DRCompilationUnit>, useful for model manipulation before code dumping"

	self addPrimitives.
	^ self compilationUnit
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> generateBuildModelAndCompileIn: aClass [ 
	^ self subclassResponsibility
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> interpreterPrimitiveTable [

	self subclassResponsibility 
]

{ #category : #testing }
DRAbstractCompilerBuilder >> isValidPrimitiveName: aString [

	^ aString isNumber not
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> newClassNamed: aString superclass: aSuperclass [
	"Answer a new <StackToRegisterMappingCogit> subclass named aString"

	^ Smalltalk image classInstaller make: [ : builder |
			builder
				name: aString;
				superclass: aSuperclass;
				category: aString ]
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> newPrimitive: aString [

	self subclassResponsibility
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> newRandomInterpreterClassName [
	"Answer a new class name to be used as interpreter containing a #basicPrimitiveTable"

	^ String streamContents: [ : stream |
		stream 
			<< self newclassPrefix;
			<< (UUIDGenerator next asString copyUpTo: $-) ]
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> newRandomTargetClass [

	^ Smalltalk image classInstaller make: [ : builder |
			builder
				name: self newRandomInterpreterClassName;
				superclass: self targetSuperclass;
				category: self newRandomInterpreterClassName ].
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> newclassPrefix [

	self subclassResponsibility
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> primitiveTable [
	^ self subclassResponsibility
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> selectBytecodes: selectBlock [
	" Answer a <Collection> of <CompiledMethod> with bytecodes matching conditions in selectBlock "

	self shouldBeImplemented
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> selectPrimitives: selectBlock [
	" Answer a <Collection> of <CompiledMethod> representing primitive methods and matching conditions in selectBlock "
	| primitiveCandidates |

	primitiveCandidates := self
		matchMethod: #beginsWith:;
		scanSuperclasses: true;
		allowDuplicates: false;
		methodsFromPattern: 'prim'.
	^ primitiveCandidates select: selectBlock
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> targetSuperclass [

	^ self compilationUnit targetSuperclass
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> targetSuperclass: anObject [

	self compilationUnit targetSuperclass: anObject
]
