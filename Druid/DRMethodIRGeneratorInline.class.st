Class {
	#name : #DRMethodIRGeneratorInline,
	#superclass : #DRMethodIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #building }
DRMethodIRGeneratorInline >> newFirstBasicBlock [

	^ self newBasicBlock
]
