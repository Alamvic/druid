Class {
	#name : #DRCompilationBuildUnit,
	#superclass : #DRCompilationUnit,
	#instVars : [
		'primitiveMethods'
	],
	#category : #'Druid-InterpreterBuilder'
}

{ #category : #'accessing - compiler' }
DRCompilationBuildUnit >> buildInitializePrimitiveTable [
	"Install the initialize primitive table method in the receiver's JIT compiler class"

	self addPrimitiveEntries.
	self primitiveMethods addAll: (self primitiveTableInitializer collectAllMethodsOn: self jitCompilerClass class)
]

{ #category : #'accessing - compiler' }
DRCompilationBuildUnit >> compileAll [
	"Write primitive methods in the receiver's JIT compiler class"

	self primitives
			do: [ : primitive | self compilePrimitive: primitive ]
			displayingProgress: 'Compiling primitives'.
	self buildInitializePrimitiveTable
]

{ #category : #accessing }
DRCompilationBuildUnit >> primitiveMethods [

	^ primitiveMethods
		ifNil: [ primitiveMethods := OrderedCollection new ]
]

{ #category : #accessing }
DRCompilationBuildUnit >> primitiveMethods: anObject [

	primitiveMethods := anObject
]
