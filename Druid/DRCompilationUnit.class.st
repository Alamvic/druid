"
It represents a Unit of compilation (a class).

"
Class {
	#name : #DRCompilationUnit,
	#superclass : #Object,
	#instVars : [
		'primitives',
		'jitCompilerClass',
		'interpreter'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRCompilationUnit >> addPrimitive: aDRPrimitiveObject [

	self primitives add: aDRPrimitiveObject 
]

{ #category : #accessing }
DRCompilationUnit >> compileAll [

	self primitives 
		do: [ : primitive | self compilePrimitive: primitive ]
		
]

{ #category : #accessing }
DRCompilationUnit >> compilePrimitive: aDRPrimitiveObject [ 

	| selector |
	selector := aDRPrimitiveObject sourceSelector.
	(self compiler 
		interpreter: self interpreter;
		primitiveName: selector;
		primitiveCompilerName: #gen_ , selector)
			compile.
		"	self objectRepresentation perform: ('genPrimitive', selector, endPart) asSymbol."
		"	generateTargetASTFromIR: aDRPrimitiveObject cfg"
]

{ #category : #accessing }
DRCompilationUnit >> compiler [

	^ DRPrimitiveCompilerCompiler new
		compilerClass: self jitCompilerClass;
		yourself
]

{ #category : #testing }
DRCompilationUnit >> hasPrimitives [
	"Answer <true> if the receiver already contains primitives"

	^ self primitives notEmpty
]

{ #category : #accessing }
DRCompilationUnit >> interpreter [

	^ interpreter
]

{ #category : #accessing }
DRCompilationUnit >> interpreter: aDRBasicInterpreter [ 

	interpreter := aDRBasicInterpreter
]

{ #category : #accessing }
DRCompilationUnit >> jitCompilerClass [
	"Answer a <Class> which is the where the JITed methods will be stored"

	^ jitCompilerClass
]

{ #category : #accessing }
DRCompilationUnit >> jitCompilerClass: aClass [

	jitCompilerClass := aClass
]

{ #category : #accessing }
DRCompilationUnit >> primitives [
	"Answer a <Collection> of IR of primitive methods, instances of <DRPrimitiveControlFlowGraph>"

	^ primitives
		ifNil: [ primitives := OrderedCollection new ]
]
