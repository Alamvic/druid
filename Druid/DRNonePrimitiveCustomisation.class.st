Class {
	#name : #DRNonePrimitiveCustomisation,
	#superclass : #DRPrimitiveCustomisation,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRNonePrimitiveCustomisation class >> pragmaName [

	^ #None
]

{ #category : #api }
DRNonePrimitiveCustomisation >> generateCustomisedPreamble: aGeneratorMethodBuilder [

	"Do nothing"
]

{ #category : #api }
DRNonePrimitiveCustomisation >> optimize: anIR [

	"Do nothing"
]
