Class {
	#name : #DRAbstractMemoryAddress,
	#superclass : #DRValue,
	#category : #'Druid-IR'
}

{ #category : #testing }
DRAbstractMemoryAddress >> isMemoryAddress [
	
	^ true
]

{ #category : #cogit }
DRAbstractMemoryAddress >> rtlArgumentExpressions [
	
	self subclassResponsibility
]
