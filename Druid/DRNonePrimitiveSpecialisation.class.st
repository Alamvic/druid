Class {
	#name : #DRNonePrimitiveSpecialisation,
	#superclass : #DRPrimitiveSpecialisation,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'as yet unclassified' }
DRNonePrimitiveSpecialisation class >> pragmaName [

	^ #none
]

{ #category : #api }
DRNonePrimitiveSpecialisation >> addInitialConstraint: anIR [

	"Do nothing"
]

{ #category : #api }
DRNonePrimitiveSpecialisation >> specialiseIR: aDRMethodIRGenerator [
	"Do nothing"

	
]
