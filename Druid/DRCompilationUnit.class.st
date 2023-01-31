"
It represents a Unit of compilation (a class).

"
Class {
	#name : #DRCompilationUnit,
	#superclass : #Object,
	#instVars : [
		'primitives'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRCompilationUnit >> addPrimitive: aDRPrimitiveControlFlowGraph [ 

	self primitives add: aDRPrimitiveControlFlowGraph 
]

{ #category : #accessing }
DRCompilationUnit >> primitives [
	"Answer a <Collection> of IR of primitive methods, instances of <DRPrimitiveControlFlowGraph>"

	^ primitives
		ifNil: [ primitives := OrderedCollection new ]
]
