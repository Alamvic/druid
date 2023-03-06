Class {
	#name : #DRAbstractCompilerBuilder,
	#superclass : #Object,
	#instVars : [
		'compilationUnit'
	],
	#category : #'Druid-InterpreterBuilder'
}

{ #category : #NEW }
DRAbstractCompilerBuilder >> addPrimitive: aString [

	(self isValidPrimitiveName: aString)
		ifTrue: [ self compilationUnit addPrimitive: (self newPrimitive: aString) ]
]

{ #category : #DELETE }
DRAbstractCompilerBuilder >> addPrimitive: aString to: aDRCompilationUnit [

	(self isValidPrimitiveName: aString)
		ifTrue: [ aDRCompilationUnit addPrimitive: (self newPrimitive: aString) ]
]

{ #category : #NEW }
DRAbstractCompilerBuilder >> addPrimitives [
	"Iterate over the receiver's interpreter (assumed to contain primitive methods) and add them to compilationUnit"

	self interpreterPrimitiveTableSelectors
		do: [ : prim | self addPrimitive: prim ]
		displayingProgress: [ : prim | 'Adding primitive: ' , prim asString ]
]

{ #category : #DELETE }
DRAbstractCompilerBuilder >> addPrimitivesTo: aCompilationUnit [
	"Iterate over the receiver's interpreter (assumed to contain primitive methods) and add them to compilationUnit"

	self interpreterPrimitiveTableSelectors
		do: [ : prim | self addPrimitive: prim to: aCompilationUnit ]
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
		ifAbsent: [ self newClassNamed: aClass superclass: self targetSuperclass ]
]

{ #category : #DELETE }
DRAbstractCompilerBuilder >> generateBuildModel [
	"Answer a <DRCompilationUnit>, useful for model manipulation before code dumping"

	self addPrimitives.
	^ self compilationUnit
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> interpreterPrimitiveTable [

	self subclassResponsibility 
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> interpreterPrimitiveTableSelectors [
	"Answer a <Collection> of <Symbol> each one representing a selector (e.g. #primitiveEqual) of the receiver's primitive table"

	^ self interpreterPrimitiveTable reject: #isNumber
]

{ #category : #testing }
DRAbstractCompilerBuilder >> isValidPrimitiveName: aString [

	^ aString ~= 0
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
DRAbstractCompilerBuilder >> targetSuperclass [

	^ self compilationUnit targetSuperclass
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> targetSuperclass: anObject [

	self compilationUnit targetSuperclass: anObject
]
