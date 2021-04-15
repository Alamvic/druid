Class {
	#name : #DROr,
	#superclass : #DRInstruction,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #visiting }
DROr >> acceptVisitor: aVisitor [

	^ aVisitor visitOr: self
]
