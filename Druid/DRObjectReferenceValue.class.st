Class {
	#name : #DRObjectReferenceValue,
	#superclass : #DRValue,
	#category : #'Druid-IR'
}

{ #category : #'cogit-generation' }
DRObjectReferenceValue >> rtlOperandQualifier [

	^ 'Cw:'
]

{ #category : #sccp }
DRObjectReferenceValue >> sccpEvaluateFor: aDRSCCP [ 

	^ self value
]
