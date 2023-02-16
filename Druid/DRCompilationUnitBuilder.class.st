Class {
	#name : #DRCompilationUnitBuilder,
	#superclass : #DRCompilationUnit,
	#instVars : [
		'primitiveMethods'
	],
	#category : #'Druid-InterpreterBuilder'
}

{ #category : #'accessing - compiler' }
DRCompilationUnitBuilder >> buildInitializePrimitiveTable [
	"Install the initialize primitive table method in the receiver's JIT compiler class"

	self addPrimitiveEntries.
	self primitiveMethods addAll: (self primitiveTableInitializer collectAllMethodsOn: self jitCompilerClass class)
]

{ #category : #'accessing - compiler' }
DRCompilationUnitBuilder >> compileAll [
	"Write primitive methods in the receiver's JIT compiler class"

	self primitives
			do: [ : primitive | self compilePrimitive: primitive ]
			displayingProgress: 'Compiling primitives'.
	self buildInitializePrimitiveTable
]

{ #category : #accessing }
DRCompilationUnitBuilder >> primitiveMethods [

	^ primitiveMethods
		ifNil: [ primitiveMethods := OrderedCollection new ]
]

{ #category : #accessing }
DRCompilationUnitBuilder >> primitiveMethods: anObject [

	primitiveMethods := anObject
]
