Class {
	#name : #DRPush,
	#superclass : #DROneArgumentInstruction,
	#category : #'Druid-Instructions'
}

{ #category : #visiting }
DRPush >> accept: aTranslator [ 

	aTranslator visitPush: self.
]

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

	valueRegister := valueRegister updateLogicalRegisters: mappings
]
