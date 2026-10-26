Extension { #name : 'DRInstructionFactory' }

{ #category : '*Druid-Opal' }
DRInstructionFactory >> initializePragma [
	
	^ DRInitializePragma pragmas.
]

{ #category : '*Druid-Opal' }
DRInstructionFactory >> initializeScope: aDRScope [

	^ DRInitializeScope scope: aDRScope
]

{ #category : '*Druid-Opal' }
DRInstructionFactory >> loadMethodArgument: index [

	^ DRBCLoadArgument
		  operands: { index }
		  result: self allocateTemporaryRegister
]

{ #category : '*Druid-Opal' }
DRInstructionFactory >> loadReceiver [

	^ DRBCLoadReceiver result: self allocateTemporaryRegister
]
