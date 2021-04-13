Class {
	#name : #DRTrueValue,
	#superclass : #DRValue,
	#category : #'Druid-IR'
}

{ #category : #'cogit-generation' }
DRTrueValue >> name [
	
	^ 'objectMemory trueObject'
]

{ #category : #'cogit-generation' }
DRTrueValue >> rtlOperandQualifier [

	^ 'Cw:'
]
