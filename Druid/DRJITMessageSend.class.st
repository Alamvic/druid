Class {
	#name : #DRJITMessageSend,
	#superclass : #DRInstruction,
	#category : #'Druid-BytecodeToJITCompilation'
}

{ #category : #visiting }
DRJITMessageSend >> acceptVisitor: aVisitor [

	^ aVisitor visitJITMessageSend: self
]

{ #category : #accessing }
DRJITMessageSend >> cogitSelectorIndex [
	
	"Special selectors are negative and 1 based in cogit.
	So they go from -1 to -32"
	^ self isSpecialSelectorSend
		ifTrue: [ (self selectorIndex + 1) negated ]
		ifFalse: [ self selectorIndex ]
]

{ #category : #testing }
DRJITMessageSend >> isJITMessageSend [
	
	^ true
]

{ #category : #printing }
DRJITMessageSend >> isMandatoryInstruction [

	"It represents a potential side effect!"
	^ true	
]

{ #category : #accessing }
DRJITMessageSend >> isSpecialSelectorSend [
		
	^ operands last value
]

{ #category : #accessing }
DRJITMessageSend >> numberOfArguments [
	
	^ operands second
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
