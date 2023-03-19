Class {
	#name : #DRStackCompilationUnit,
	#superclass : #DRCompilationUnit,
	#category : #'Druid-InterpreterBuilder'
}

{ #category : #'accessing - compiler' }
DRStackCompilationUnit >> addPrimitiveEntry: aDRPrimitiveObject [
	"Add a primitive entry compatible with the expected format from #initializePrimitiveTable implementation"

	self initPrimitiveTable add: {
		aDRPrimitiveObject primitiveNumber .
		aDRPrimitiveObject sourceSelector }
]

{ #category : #'accessing - compiler' }
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
