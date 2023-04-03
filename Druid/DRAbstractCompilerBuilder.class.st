"
Abstract class providing: 

- A primitives selection protocol through `DRAbstractCompilerBuilder>>#selectPrimitives:`
- A compilation unit `DRCompilationUnit`.
- Convenience methods to create classes on demand: `DRAbstractCompilerBuilder>>#environmentAt:`

"
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

	^ DRCogitCompilationUnit 
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
DRAbstractCompilerBuilder >> newClassNamed: aString superclass: aSuperclass [
	"Answer a new <StackToRegisterMappingCogit> subclass named aString"

	^ Smalltalk image classInstaller make: [ : builder |
			builder
				name: aString;
				superclass: aSuperclass;
				category: aString ]
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> targetClass [
	" Answer the <Class> which we are building "

	^ self compilationUnit targetClass
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> targetClass: aClass [
	" Set the <Class> which we are building "

	^ self compilationUnit targetClass: aClass
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> targetSuperclass [

	^ self compilationUnit targetSuperclass
]

{ #category : #accessing }
DRAbstractCompilerBuilder >> targetSuperclass: anObject [

	self compilationUnit targetSuperclass: anObject
]
