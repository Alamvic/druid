Class {
	#name : #DRLoad,
	#superclass : #DROneArgumentInstruction,
	#category : #'Druid-Instructions'
}

{ #category : #'instance-creation' }
DRLoad class >> basePointerRegister [
	
	^ self value: DRPhysicalRegister basePointer
]

{ #category : #printing }
DRLoad >> mnemonic [
	
	^ 'LOAD'
]

{ #category : #accessing }
DRLoad >> registers [
	
	^ valueRegister registers , result registers
]

{ #category : #'register allocation' }
DRLoad >> updateLogicalRegisters: mappings [ 

	result := result updateLogicalRegisters: mappings	.
	valueRegister := valueRegister updateLogicalRegisters: mappings
]
