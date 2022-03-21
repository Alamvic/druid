Class {
	#name : #DRMemoryAddress,
	#superclass : #DRValue,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #testing }
DRMemoryAddress >> isMemoryAddress [
	
	^ true
]

{ #category : #printing }
DRMemoryAddress >> name [
	
	^ 'M0'
]

{ #category : #'cogit-generation' }
DRMemoryAddress >> rtlOperandQualifier [
	
	^ 'R:'
]
