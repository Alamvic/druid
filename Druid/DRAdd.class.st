Class {
	#name : #DRAdd,
	#superclass : #DRInstruction,
	#instVars : [
		'leftOperandRegister',
		'rightOperandRegister',
		'leftOperandInstruction',
		'rightOperandInstruction'
	],
	#category : #'Druid-Instructions'
}

{ #category : #visiting }
DRAdd >> accept: aMCTranslator [ 

	aMCTranslator visitAdd: self
]

{ #category : #accessing }
DRAdd >> leftOperand: anObject [

	leftOperandInstruction := anObject.
	leftOperandRegister := anObject
]

{ #category : #accessing }
DRAdd >> leftOperandRegister [
	^ leftOperandRegister
]

{ #category : #printing }
DRAdd >> mnemonic [
	
	^ 'ADD'
]

{ #category : #accessing }
DRAdd >> operands [

	^ { leftOperandRegister . rightOperandRegister }
]

{ #category : #accessing }
DRAdd >> registers [
	
	^ leftOperandRegister registers , rightOperandRegister registers, result registers
]

{ #category : #accessing }
DRAdd >> rightOperand: anObject [

	rightOperandInstruction := anObject.
	rightOperandRegister := anObject
]

{ #category : #accessing }
DRAdd >> rightOperandRegister [
	^ rightOperandRegister
]

{ #category : #'register allocation' }
DRAdd >> updateLogicalRegisters: mappings [ 

	leftOperandRegister := leftOperandRegister updateLogicalRegisters: mappings.
	rightOperandRegister := rightOperandRegister updateLogicalRegisters: mappings.
	result := result updateLogicalRegisters: mappings.
]
