Class {
	#name : #DRPrimitiveFail,
	#superclass : #DRNoop,
	#category : #'Druid-IR'
}

{ #category : #testing }
DRPrimitiveFail >> isMandatoryInstruction [

	^ true
]

{ #category : #testing }
DRPrimitiveFail >> isPrimitiveFail [

	^ true
]

{ #category : #testing }
DRPrimitiveFail >> opcode [
	
	^ 'PrimitiveFail'
]
