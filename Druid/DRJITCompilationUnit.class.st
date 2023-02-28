Class {
	#name : #DRJITCompilationUnit,
	#superclass : #DRCompilationUnit,
	#category : #'Druid-InterpreterBuilder'
}

{ #category : #'accessing - compiler' }
DRJITCompilationUnit >> addPrimitiveEntry: aDRPrimitiveObject [
	"Add a primitive entry compatible with the expected format from #initializePrimitiveTable implementation"

	self initPrimitiveTable add: {
		aDRPrimitiveObject primitiveNumber .
		aDRPrimitiveObject genSelector .
		aDRPrimitiveObject argumentCount > 0
			ifTrue: [ aDRPrimitiveObject argumentCount ] }
]
