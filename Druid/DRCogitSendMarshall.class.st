Class {
	#name : #DRCogitSendMarshall,
	#superclass : #DRInstruction,
	#category : #'Druid-BytecodeToJITCompilation'
}

{ #category : #instructions }
DRCogitSendMarshall class >> send: aSend [

	^ self operands: { aSend } result: DRNoRegister new
]

{ #category : #visiting }
DRCogitSendMarshall >> acceptVisitor: aVisitor [

	aVisitor visitCogitSendMarshall: self
]

{ #category : #testing }
DRCogitSendMarshall >> isCogitSendMarshall [
	
	^ true
]

{ #category : #accessing }
DRCogitSendMarshall >> numberOfArguments [
	
	^ self send numberOfArguments
]

{ #category : #printing }
DRCogitSendMarshall >> opcode [
	
	^ 'CogitMarshallSend'
]

{ #category : #accessing }
DRCogitSendMarshall >> operands [

	^ #()
]

{ #category : #accessing }
DRCogitSendMarshall >> send [
	
	^ self operand1
]
