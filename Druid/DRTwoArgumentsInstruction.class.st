Class {
	#name : #DRTwoArgumentsInstruction,
	#superclass : #DRInstruction,
	#instVars : [
		'leftOperandRegister',
		'rightOperandRegister',
		'leftOperandInstruction',
		'rightOperandInstruction'
	],
	#category : #'Druid-Instructions'
}

{ #category : #accessing }
DRTwoArgumentsInstruction >> leftOperand: anObject [

	leftOperandInstruction := anObject.
	leftOperandRegister := anObject
]

{ #category : #accessing }
DRTwoArgumentsInstruction >> leftOperandRegister [
	^ leftOperandRegister
]

{ #category : #accessing }
DRTwoArgumentsInstruction >> operands [

	^ { leftOperandRegister . rightOperandRegister }
]

{ #category : #accessing }
DRTwoArgumentsInstruction >> registers [
	
	^ leftOperandRegister registers , rightOperandRegister registers, result registers
]

{ #category : #accessing }
DRTwoArgumentsInstruction >> rightOperand: anObject [

	rightOperandInstruction := anObject.
	rightOperandRegister := anObject
]

{ #category : #accessing }
DRTwoArgumentsInstruction >> rightOperandRegister [
	^ rightOperandRegister
]

{ #category : #'register allocation' }
DRTwoArgumentsInstruction >> updateLogicalRegisters: mappings [ 

	leftOperandRegister := leftOperandRegister updateLogicalRegisters: mappings.
	rightOperandRegister := rightOperandRegister updateLogicalRegisters: mappings.
	result := result updateLogicalRegisters: mappings.
]
