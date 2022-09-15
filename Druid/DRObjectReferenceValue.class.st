Class {
	#name : #DRObjectReferenceValue,
	#superclass : #DRValue,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRObjectReferenceValue >> acceptVisitor: aVisitor [

	^ aVisitor visitConstant: self
]

{ #category : #testing }
DRObjectReferenceValue >> isConstant [

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
	
	^ DRIntegerType new
]
