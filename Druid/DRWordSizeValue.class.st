Class {
	#name : 'DRWordSizeValue',
	#superclass : 'DRValue',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'cogit-generation' }
DRWordSizeValue >> name [
	
	^ 'objectMemory wordSize'
]

{ #category : 'cogit-generation' }
DRWordSizeValue >> rtlOperandQualifier [

	^ 'Cq:'
]
