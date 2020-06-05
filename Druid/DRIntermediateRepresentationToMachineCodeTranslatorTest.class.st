Class {
	#name : #DRIntermediateRepresentationToMachineCodeTranslatorTest,
	#superclass : #VMSimpleStackBasedCogitBytecodeTest,
	#pools : [
		'CogRTLOpcodes'
	],
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRIntermediateRepresentationToMachineCodeTranslatorTest >> testPushConstantInstructionGeneratesAPush [

	| builder mcTranslator |
	builder := DRIRBuilder new.
	builder push: (DRConstant value: 17).
	builder assignPhysicalRegisters.

	mcTranslator := DRIntermediateRepresentationToMachineCodeTranslator
		translate: builder instructions withCompiler: cogit.
	self assert: mcTranslator instructions first opcode equals: PushCw
]

{ #category : #tests }
DRIntermediateRepresentationToMachineCodeTranslatorTest >> testPushConstantInstructionGeneratesAPushWithCorrectOperand [

	| builder mcTranslator |
	builder := DRIRBuilder new.
	builder push: (DRConstant value: 17).
	builder assignPhysicalRegisters.

	mcTranslator := DRIntermediateRepresentationToMachineCodeTranslator
		translate: builder instructions withCompiler: cogit.

	self assert: mcTranslator instructions first operands first equals: 17	
]

{ #category : #tests }
DRIntermediateRepresentationToMachineCodeTranslatorTest >> testPushConstantInstructionGeneratesASingleInstruction [

	| builder mcTranslator |
	builder := DRIRBuilder new.
	builder push: (DRConstant value: 17).
	builder assignPhysicalRegisters.

	mcTranslator := DRIntermediateRepresentationToMachineCodeTranslator translate: builder instructions withCompiler: cogit.
	self assert: mcTranslator instructions size equals: 1	
]

{ #category : #tests }
DRIntermediateRepresentationToMachineCodeTranslatorTest >> testPushRegisterInstructionGeneratesAPush [

	| builder mcTranslator |
	builder := DRIRBuilder new.
	builder push: (DRLogicalRegister named: 'l1').
	builder assignPhysicalRegisters.

	mcTranslator := DRIntermediateRepresentationToMachineCodeTranslator translate: builder instructions withCompiler: cogit.
	self assert: mcTranslator instructions first opcode equals: PushR
]
