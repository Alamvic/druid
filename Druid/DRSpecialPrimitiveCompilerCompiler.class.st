Class {
	#name : #DRSpecialPrimitiveCompilerCompiler,
	#superclass : #DRPrimitiveCompilerCompiler,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'generation-IR' }
DRSpecialPrimitiveCompilerCompiler >> configureIRBuilder: generator forMethod: method [

	super configureIRBuilder: generator forMethod: method.

	generator specializeForReceiverFormat: (self specializedFormat: method)
]

{ #category : #'generation-IR' }
DRSpecialPrimitiveCompilerCompiler >> newIRGenerator [

	^ DRSpecializedPrimitiveIRGenerator new
]

{ #category : #query }
DRSpecialPrimitiveCompilerCompiler >> specializedFormat: method [

	^ (method ast pragmaNamed: #specilizeFormat:) arguments first value
]
