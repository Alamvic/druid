Class {
	#name : #DRCogitTrueObject,
	#superclass : #DRValue,
	#category : #'Druid-IR'
}

{ #category : #'cogit-generation' }
DRCogitTrueObject >> name [
	
	^ 'objectMemory trueObject'
]

{ #category : #'cogit-generation' }
DRCogitTrueObject >> rtlOperandQualifier [

	^ 'Cw:'
]
