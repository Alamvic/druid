Class {
	#name : #DRNonePrimitiveSpecialisation,
	#superclass : #DRPrimitiveSpecialisation,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRNonePrimitiveSpecialisation class >> pragmaName [

	^ #none
]

{ #category : #api }
DRNonePrimitiveSpecialisation >> specialiseIR: aDRMethodIRGenerator [
	"Do nothing"

	
]
