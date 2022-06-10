Class {
	#name : #DRObjectReferenceValue,
	#superclass : #DRValue,
	#category : #'Druid-IR'
}

{ #category : #testing }
DRObjectReferenceValue >> isObjectReferenceValue [

	^ true
]

{ #category : #'cogit-generation' }
DRObjectReferenceValue >> rtlOperandQualifier [

	^ 'Cw:'
]

{ #category : #sccp }
DRObjectReferenceValue >> sccpLatticeValueFor: aDRSCCP [ 

	^ self value
]

{ #category : #'type based splitting' }
DRObjectReferenceValue >> typeInTypeTable: aCollection [ 
	
	^ (SmallInteger minVal to: SmallInteger maxVal) asDRType
]
