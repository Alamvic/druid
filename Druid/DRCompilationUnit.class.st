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

	^ primitives 
		ifNil: [ primitives := OrderedCollection new ]	         
]
