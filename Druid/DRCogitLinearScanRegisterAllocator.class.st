Class {
	#name : #DRCogitLinearScanRegisterAllocator,
	#superclass : #DRLinearScanRegisterAllocator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'live-analysis' }
DRCogitLinearScanRegisterAllocator >> buildLiveSets [

	| shouldRecompute |
	[ super buildLiveSets.
	shouldRecompute := self coalesceIntervalsForTwoAddressCode ]
		doWhileTrue: [ shouldRecompute ]
]

{ #category : #'live-analysis' }
DRCogitLinearScanRegisterAllocator >> coalesceIntervalsForTwoAddressCode [

	"2 address code transformation. Second operand and destination MUST have the same assigned register.
	If their live intervals do not overlap => merge them.
	Otherwise => split the intervals by creating a copy instruction"

	self instructionsDo: [ :i |
	
		"We should have a special case for phi functions"		
		i isPhiFunction ifFalse: [
		
		"If one of the operands is a physical registers, it gives us an additional constraint that could prevent the 2AC transformation.
		Introduce a copy instruction to cut the instruction in two different and remove this constraint.
		Then exit and recompute live sets with the new instruction.
		
		R1 := RP + 1.
		=>
		R2 := RP.
		R1 := R2 + 1
		"
		(i isComparison not and: [i operands size = 2 and: [ i operands anySatisfy: [ :op | op isPhysicalRegister ] ]]) ifTrue: [
			| registerToCopy copyInstruction |
			registerToCopy := i operands detect: [ :op | op isPhysicalRegister ].
			
			"Create a copy instruction before, branch it, and return true to indicate we need to recompute intervals"
			copyInstruction := i controlFlowGraph instructionFactory copy: registerToCopy.
			i addBefore: copyInstruction.
			i replaceOperand: registerToCopy by: copyInstruction.
			^ true
		].
		
		(i isComparison not and: [i operands size = 2]) ifTrue: [ | secondOperandInterval currentInterval operandToMergeInterval |
			"Merge the two intervals, finding the operand that is not a constant.
			Take the firt operand if both are not constants.
			When transforming to Cogit code, we output the second operand and the destination"
			operandToMergeInterval := i operand1 isInstruction
				ifTrue: [ i operand1 ]
				ifFalse: [ i operand2 ].
			
			secondOperandInterval := self liveSetOf: operandToMergeInterval.
			currentInterval := self liveSetOf: i.

			(secondOperandInterval overlaps: currentInterval)
				ifTrue: [ | copyInstruction |
					"Create a copy instruction before, branch it, and return true to indicate we need to recompute intervals"
					copyInstruction := i controlFlowGraph instructionFactory copy: operandToMergeInterval.
					i addBefore: copyInstruction.
					i replaceOperand: operandToMergeInterval by: copyInstruction.
					^ true
				]
				ifFalse: [ | mergedInterval |
					"Create a merged interval and set it as interval for the current definition and its second operand."
					mergedInterval := secondOperandInterval mergeWith: currentInterval.
					liveSets keys do: [ :k |
						({ secondOperandInterval . currentInterval } includes: (liveSets at: k))
							ifTrue: [ liveSets at: k put: mergedInterval ]
					] ] ] ] ].
		^ false
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
		DRPhysicalRegister name: 'Extra2Reg' }.
	self spillRegisters: { 
		DRPhysicalRegister name: 'Extra3Reg'.
		DRPhysicalRegister name: 'Extra4Reg' }
]

{ #category : #'live-analysis' }
DRCogitLinearScanRegisterAllocator >> initializeLiveSetOfInstruction: anInstruction [

	"If the instruction is a division or modulo, this will be translated in cogit's DIVR:R:Quo:Rem: instruction.
	This instruction requires 4 different registers and not three (operands + destination).
	We model this by adding an extra operand at this point."

	"First register the live interval for the division itself so it does not overlap"
	super initializeLiveSetOfInstruction: anInstruction.
		
	"Then regiter the live interval for the new synthetic operand"
	(anInstruction isDivision or: [ anInstruction isModulo ]) ifTrue: [ | newOperand |
		newOperand := anInstruction basicBlock controlFlowGraph allocateTemporaryRegister.
		"This live set starts in the next instruction, but it is not used so it will be freed"
		(self liveSetOf: newOperand) addRangeFrom: anInstruction to: anInstruction nextInstruction.
		anInstruction addOperand: newOperand ].
	

]

{ #category : #'live-analysis' }
DRCogitLinearScanRegisterAllocator >> updateLiveSet: liveSet ofOperand: op ofInstruction: anInstruction inBlock: b [

	"If the instruction is a division or modulo, this will be translated in cogit's DIVR:R:Quo:Rem: instruction.
	This instruction requires 4 different registers and not three (operands + destination).
	We model this by adding an extra operand at this point."

	"We need to ignore the last operand"
	((anInstruction isDivision or: [ anInstruction isModulo ]) and: [ 
		 op = anInstruction operands last ]) ifTrue: [ ^ self ].

	^ super
		  updateLiveSet: liveSet
		  ofOperand: op
		  ofInstruction: anInstruction
		  inBlock: b
]

{ #category : #'live-analysis' }
DRCogitLinearScanRegisterAllocator >> updateLiveSetOfOperand: op ofInstruction: anInstruction inBlock: b [

	"If the instruction is a division or modulo, this will be translated in cogit's DIVR:R:Quo:Rem: instruction.
	This instruction requires 4 different registers and not three (operands + destination).
	We model this by adding an extra operand at this point."

	
	(anInstruction isDivision or: [ anInstruction isModulo ]) ifFalse: [ ^ self ].
	"We need to ignore the last operand"
	op = anInstruction operands last ifTrue: [ ^ self ].
	
	^ super updateLiveSetOfOperand: op ofInstruction: anInstruction inBlock: b
]
