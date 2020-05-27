Class {
	#name : #DRPush,
	#superclass : #DROneArgumentInstruction,
	#category : #'Druid-Instructions'
}

{ #category : #printing }
DRPush >> mnemonic [
	
	^ 'PUSH'
]

{ #category : #accessing }
DRPush >> registers [
	^ valueRegister registers
]

{ #category : #'register allocation' }
DRPush >> updateLogicalRegisters: mappings [ 

	self assert: mappings size = 1.
	valueRegister := mappings associations first value.
]
