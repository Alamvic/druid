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

{ #category : #compiling }
DRStackCompilationUnit >> compileInitializePrimitiveTable [
	"Install the initialize primitive table method in the receiver's JIT compiler class"

	self addPrimitiveEntries.
	self dispatchTableBuilder 
		primitiveSpec: initPrimitiveTable;
		installAllMethodsOn: self targetClass class
]
