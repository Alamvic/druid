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

{ #category : #initialization }
DRCogitLinearScanRegisterAllocator >> initialize [

	super initialize.
	self registers: { 
		DRPhysicalRegister name: 'TempReg'.
		DRPhysicalRegister name: 'ClassReg'.
		DRPhysicalRegister name: 'SendNumArgsReg'.
		DRPhysicalRegister name: 'Extra0Reg'.
		DRPhysicalRegister name: 'Extra1Reg'.
		DRPhysicalRegister name: 'Extra2Reg'.
		DRPhysicalRegister name: 'Extra3Reg'.
		DRPhysicalRegister name: 'Extra4Reg' }.
	self spillRegisters: { 
		DRPhysicalRegister name: 'Extra3Reg'.
		DRPhysicalRegister name: 'Extra4Reg' }
]
