Class {
	#name : #DRStackCompilationUnit,
	#superclass : #DRInterpreterCompilationUnit,
	#instVars : [
		'primitiveIndex'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #'accessing - primitives' }
DRStackCompilationUnit >> addPrimitiveEntries [
	"As part of the build unit, add the receiver's primitives from its <Collection> of <DRPrimitiveObject>"

	self addSpecialPrimitives.
	self primitives do: [ :primitiveObject |
		self addPrimitiveEntry: primitiveObject ]
]

{ #category : #'accessing - primitives' }
DRStackCompilationUnit >> addPrimitiveEntry: aDRPrimitiveObject [
	"Add a primitive entry compatible with the expected format from #initializePrimitiveTable implementation"

	self primitiveTable add: {
		aDRPrimitiveObject primitiveNumber .
		aDRPrimitiveObject sourceSelector }
]

{ #category : #'accessing - primitives' }
DRStackCompilationUnit >> addSpecialPrimitives [
	"Private - This is needed so to conform the primitive array 'specification'. Read AbstractInterpreter class>>tabble:from: side for details"

	self primitiveTable add: {
		0 . 0 }
]

{ #category : #'accessing - compiler' }
DRStackCompilationUnit >> compileAll [

	self addPrimitiveEntries.
	self dispatchTableGenerator
		primitiveSpec: primitiveTable;
		installAllMethodsOn: self targetClass class
]

{ #category : #private }
DRStackCompilationUnit >> dispatchTableGeneratorClass [

	^ DRStackInterpreterDispatchTableGenerator
]
