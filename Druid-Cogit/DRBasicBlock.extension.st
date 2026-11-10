Extension { #name : 'DRBasicBlock' }

{ #category : '*Druid-Cogit' }
DRBasicBlock >> loadArgument: index [

	^ self addInstruction: (self instructionFactory loadArgument: index)
]

{ #category : '*Druid-Cogit' }
DRBasicBlock >> loadCogitReceiver [

	^ self addInstruction: self instructionFactory loadCogitReceiver
]
