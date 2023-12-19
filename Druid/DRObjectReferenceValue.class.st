Class {
	#name : #DRObjectReferenceValue,
	#superclass : #DRJitCompileTimeExpression,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRObjectReferenceValue >> acceptVisitor: aVisitor [

	^ aVisitor visitObjectReferenceValue: self
]

{ #category : #testing }
DRObjectReferenceValue >> isInvariantFrom: loop [
	^ true
]

{ #category : #testing }
DRObjectReferenceValue >> isObjectReferenceValue [

	^ true
]

{ #category : #'cogit-generation' }
DRObjectReferenceValue >> rtlOperandQualifier [

	^ 'Cq:'
]

{ #category : #sccp }
DRObjectReferenceValue >> sccpLatticeValueFor: aDRSCCP [ 

	^ self value
]

{ #category : #types }
DRObjectReferenceValue >> type [
	
	^ DRPointerType new
]
