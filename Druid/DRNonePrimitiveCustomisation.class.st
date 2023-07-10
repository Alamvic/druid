Class {
	#name : #DRNonePrimitiveCustomisation,
	#superclass : #DRPrimitiveCustomisation,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'as yet unclassified' }
DRNonePrimitiveCustomisation class >> pragmaName [

	^ #none
]

{ #category : #api }
DRNonePrimitiveCustomisation >> generateCustomisedPreamble: aGeneratorMethodBuilder [

	"Do nothing"
]

{ #category : #api }
DRNonePrimitiveCustomisation >> optimize: anIR [

	"Do nothing"
]

{ #category : #'generation-IR' }
DRNonePrimitiveCustomisation >> specialiseIR: aDRMethodIRGenerator [
	"Do nothing"

	
]
