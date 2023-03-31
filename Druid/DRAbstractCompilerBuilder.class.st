Class {
	#name : #DRAbstractCompilerBuilder,
	#superclass : #Object,
	#instVars : [
		'compilationUnit',
		'interpreterClass'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #'accessing - compiler' }
DRAbstractCompilerBuilder >> compilationUnit [

	^ compilationUnit
		ifNil: [ 	compilationUnit := self compilationUnitClass fromCompilerBuilder: self ]
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
DRAbstractCompilerBuilder >> environmentAt: aClass [
	"Answer the class named aClass. Create if it doesn't exist"

	| realSuperclass |
	realSuperclass := self targetSuperclass isSymbol
		ifTrue: [ self class environment at: self targetSuperclass ]
		ifFalse: [ self targetSuperclass ].
	^ self class environment
		at: aClass
		ifAbsent: [ self newClassNamed: aClass superclass: realSuperclass ]
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> generateBuildModelAndCompileIn: aClass [ 
	^ self subclassResponsibility
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> interpreterClass [

	self subclassResponsibility
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> interpreterPrimitiveTable [

	self subclassResponsibility 
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
DRAbstractCompilerBuilder >> primitiveTable [
	^ self subclassResponsibility
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> selectPrimitives: selectBlock [
	" Answer a <Collection> of <CompiledMethod> representing primitive methods and matching conditions in selectBlock "

	^ (self primitiveTable reject: #isNumber) 
		select: [ : primSelector | selectBlock value: (self interpreterClass lookupSelector: primSelector) ]
		thenCollect: [ : primSelector | self interpreterClass lookupSelector: primSelector ]
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> targetClass [
	" Answer the <Class> which we are building "

	^ self compilationUnit targetClass
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> targetSuperclass [

	^ self compilationUnit targetSuperclass
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> targetSuperclass: anObject [

	self compilationUnit targetSuperclass: anObject
]
