Class {
	#name : 'DRCogitSendMarshall',
	#superclass : 'DRInstruction',
	#instVars : [
		'doPop'
	],
	#category : 'Druid-Cogit-IR',
	#package : 'Druid-Cogit',
	#tag : 'IR'
}

{ #category : 'instructions' }
DRCogitSendMarshall class >> send: aSend [

	^ self operands: { aSend } result: DRNoRegister new
]

{ #category : 'visiting' }
DRCogitSendMarshall >> acceptVisitor: aVisitor [

	aVisitor visitCogitSendMarshall: self
]

{ #category : 'accessing' }
DRCogitSendMarshall >> doPop: aBoolean [ 
	
	doPop := aBoolean
]

{ #category : 'testing' }
DRCogitSendMarshall >> initialize [

	super initialize.
	doPop := false
]

{ #category : 'testing' }
DRCogitSendMarshall >> isCogitSendMarshall [
	
	^ true
]

{ #category : 'testing' }
DRCogitSendMarshall >> isMandatoryInstruction [

	^ true
]

{ #category : 'accessing' }
DRCogitSendMarshall >> numberOfArguments [
	
	^ self send numberOfArguments
]

{ #category : 'printing' }
DRCogitSendMarshall >> opcode [
	
	^ 'CogitMarshallSend'
]

{ #category : 'accessing' }
DRCogitSendMarshall >> operands [

	^ #()
]

{ #category : 'SCCP' }
DRCogitSendMarshall >> sccpLatticeValueFor: sccp [ 
	
	"This instruction has no value"
	^ sccp bottom
]

{ #category : 'accessing' }
DRCogitSendMarshall >> send [
	
	^ self operand1
]

{ #category : 'asserting' }
DRCogitSendMarshall >> shouldPop [
	
	^ doPop
]
