Class {
	#name : #DRRegisterAllocationTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRRegisterAllocationTest >> testLoadConstantKeepsConstantAsValue [

	| builder loadInstruction |
	builder := DRIRBuilder new.
	loadInstruction := builder loadAddress: (DRConstant value: 17).
	
	self assert: loadInstruction value isConstant.
	
	builder assignPhysicalRegisters.
	
	self assert: loadInstruction value isConstant.

]

{ #category : #tests }
DRRegisterAllocationTest >> testLoadConstantReceivesAPhysicalRegisterAsResult [

	| builder loadInstruction |
	builder := DRIRBuilder new.
	loadInstruction := builder loadAddress: (DRConstant value: 17).
	
	self deny: loadInstruction result isPhysical.
	
	builder assignPhysicalRegisters.
	
	self assert: loadInstruction result isPhysical.

]

{ #category : #tests }
DRRegisterAllocationTest >> testLoadIndirectPhysicalRegisterDoesNotChange [

	| builder loadInstruction |
	builder := DRIRBuilder new.
	loadInstruction := builder loadAddress: (DRPhysicalRegister basePointer).
	
	self assert: loadInstruction value isPhysical.
	
	builder assignPhysicalRegisters.
	
	self assert: loadInstruction value registers equals: {DRPhysicalRegister basePointer}.

]

{ #category : #tests }
DRRegisterAllocationTest >> testLoadIndirectRegisterReceivesAPhysicalRegisterAsResult [

	| builder loadInstruction |
	builder := DRIRBuilder new.
	loadInstruction := builder loadAddress: (DRLogicalRegister named: 'l1').
	
	self deny: loadInstruction result isPhysical.
	
	builder assignPhysicalRegisters.
	
	self assert: loadInstruction result isPhysical.

]
