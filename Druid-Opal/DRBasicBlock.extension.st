Extension { #name : 'DRBasicBlock' }

{ #category : '*Druid-Opal' }
DRBasicBlock >> initializeScope: aDRScope [

	^ self addInstruction:
		  (self instructionFactory initializeScope: aDRScope)
]
