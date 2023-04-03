Class {
	#name : #DRStackCompilationUnit,
	#superclass : #DRPrimitiveCompilationUnit,
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

	self initPrimitiveTable add: {
		aDRPrimitiveObject primitiveNumber .
		aDRPrimitiveObject sourceSelector }
]

{ #category : #adding }
DRStackCompilationUnit >> addPrimitives: aCollection [ 

	(self collectPrimitives: aCollection)
		do: [ : prim | self addPrimitive: prim ]
		displayingProgress: [ : prim | 'Adding primitive: ' , prim asString ]
]

{ #category : #'accessing - primitives' }
DRStackCompilationUnit >> addSpecialPrimitives [
	"Private - This is needed so to conform the primitive array 'specification'. Read AbstractInterpreter class>>tabble:from: side for details"

	self initPrimitiveTable add: {
		0 . 0 }
]

{ #category : #'accessing - compiler' }
DRStackCompilationUnit >> compileInitializePrimitiveTable [
	"Install the initialize primitive table method in the receiver's JIT compiler class"

	self addPrimitiveEntries.
	self dispatchTableBuilder
		primitiveSpec: initPrimitiveTable;
		installAllMethodsOn: self targetClass class
]

{ #category : #private }
DRStackCompilationUnit >> dispatchTableBuilderClass [

	^ DRStackInterpreterDispatchTableBuilder
]

{ #category : #initialization }
DRStackCompilationUnit >> initialize [

	super initialize.
	primitiveIndex := 0
]

{ #category : #'accessing - primitives' }
DRStackCompilationUnit >> newPrimitive: aString [
	" Answer a new primitive object with its metadata and CFG generated "

	^ DRPrimitiveObject new
		primitiveNumber: self primitiveIndex;
		sourceSelector: aString;
		yourself
]

{ #category : #'accessing - primitives' }
DRStackCompilationUnit >> primitiveIndex [

	^ primitiveIndex :=  primitiveIndex + 1
]
