Extension { #name : 'DRIRGenerator' }

{ #category : '*Druid-Cogit' }
DRIRGenerator >> argRegisterNumber: anInteger [

	"TODO: Merge with #interpretStackValueWith: aRBMessageNode withValue: aValue ?"

	| loadArgument |
	numberOfArguments = anInteger ifTrue: [
		^ self instructionFactory loadReceiver classType:
			  (DRSignedIntegerType size: 8) ].

	loadArgument := (self instructionFactory loadArgument: anInteger)
		                classType: (DRSignedIntegerType size: 8).
	loadArgument origin: thisContext sender stack.
	self currentBasicBlock addInstruction: loadArgument.
	^ loadArgument
]
