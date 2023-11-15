Class {
	#name : #DRFlushStack,
	#superclass : #DRInstruction,
	#category : #'Druid-BytecodeToJITCompilation'
}

{ #category : #visiting }
DRFlushStack >> acceptVisitor: aVisitor [ 

	^ aVisitor visitFlushStack: self
]

{ #category : #visiting }
DRFlushStack >> isMandatoryInstruction [

	^ true
]
