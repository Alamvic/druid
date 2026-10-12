Extension { #name : 'DRBasicBlock' }

{ #category : '*Druid-Opal' }
DRBasicBlock >> initializeScope: aDRScope [

	^ self addInstruction:
		  (self instructionFactory initializeScope: aDRScope)
]

{ #category : '*Druid-Opal' }
DRBasicBlock >> loadMethodArgument: index [

	^ self addInstruction: (self instructionFactory loadMethodArgument: index)
]
