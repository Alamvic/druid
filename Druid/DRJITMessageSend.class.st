Class {
	#name : #DRJITMessageSend,
	#superclass : #DRInstruction,
	#category : #'Druid-BytecodeGeneration'
}

{ #category : #visiting }
DRJITMessageSend >> acceptVisitor: aVisitor [

	^ aVisitor visitJITMessageSend: self
]

{ #category : #printing }
DRJITMessageSend >> isMandatoryInstruction [

	"It represents a potential side effect!"
	^ true	
]

{ #category : #accessing }
DRJITMessageSend >> numberOfArguments [
	
	^ operands second value
]

{ #category : #printing }
DRJITMessageSend >> opcode [
	
	^ 'SEND'
]

{ #category : #SCCP }
DRJITMessageSend >> sccpEvaluateFor: sccp [

	"Nothing to do"

	
]

{ #category : #accessing }
DRJITMessageSend >> selectorIndex [
	
	^ operands first value
]
