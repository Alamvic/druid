Class {
	#name : #DRCogitLinearScanRegisterAllocator,
	#superclass : #DRLinearScanRegisterAllocator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'live-analysis' }
DRCogitLinearScanRegisterAllocator >> computeLiveSetsForInstruction: i [

	"If the instruction is a division or modulo, this will be translated in cogit's DIVR:R:Quo:Rem: instruction.
	This instruction requires 4 different registers and not one.
	We model this by adding an extra operand at this point."
	(i isDivision or: [ i isModulo ]) ifTrue: [ | newOperand liveSet |
		newOperand := i basicBlock controlFlowGraph allocateTemporaryRegister.
		liveSet := DRLiveSet new.
		liveSet definition: i.
		liveSets at: newOperand result put: liveSet.
		i addOperand: newOperand ].
	super computeLiveSetsForInstruction: i.
]
