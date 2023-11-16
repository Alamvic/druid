Class {
	#name : #DRCogitLinearScanRegisterAllocator,
	#superclass : #DRLinearScanRegisterAllocator,
	#category : #'Druid-Cogit'
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
		(i result isNoResult not and: [i operands size = 2 and: [ i operands anySatisfy: [ :op | op isPhysicalRegister ] ]]) ifTrue: [
			| registerToCopy copyInstruction |
			registerToCopy := i operands detect: [ :op | op isPhysicalRegister ].
			
			"Create a copy instruction before, branch it, and return true to indicate we need to recompute intervals"
			copyInstruction := i controlFlowGraph instructionFactory copy: registerToCopy.
			i addBefore: copyInstruction.
			i replaceOperand: registerToCopy by: copyInstruction.
			^ true
		].

		(i result isNoResult not and: [i operands size = 2])
			ifTrue: [ | secondOperandInterval currentInterval operandToMergeInterval |
		(i result type isFloatType not and: [ (i operands collect: #type) allSatisfy: [ :e | e isFloatType ] ])
			ifTrue: [^ false].

			"Merge the two intervals, finding the operand that is not a constant.
			Take the first operand.
			When transforming to Cogit code, we output the first operand as second operand and destination.
			
			R0 := Sub R0 1
			=>
			Sub 1 R0
			"
			operandToMergeInterval := i operand1.

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
					(liveSets keys sorted: [ :a :b | a name < b name ]) do: [ :k |
						({ secondOperandInterval . currentInterval } includes: (liveSets at: k))
							ifTrue: [ liveSets at: k put: mergedInterval ]
					] ] ] ] ].
		^ false
]

{ #category : #'live-analysis' }
DRCogitLinearScanRegisterAllocator >> computeLivenessOfInstruction: anInstruction inBlock: b withLiveSet: live [

	"If the instruction is an integer division or modulo, this will be translated in cogit's DIVR:R:Quo:Rem: instruction.
	This instruction requires 4 different registers and not three (operands + destination).
	We model this by adding an extra operand at this point."

	"Then regiter the live interval for the new synthetic operand"

	((anInstruction isDivision or: [ anInstruction isModulo ]) and: [ 
		 anInstruction type isFloatType not ]) ifTrue: [ 
		| newOperand |
		anInstruction operands size = 2
			ifTrue: [ 
				newOperand := anInstruction basicBlock controlFlowGraph
					              allocateTemporaryRegister.
				anInstruction addOperand: newOperand ]
			ifFalse: [ newOperand := anInstruction operands last ].


		"This live set starts in the next instruction, but it is not used so it will be freed"
		(self liveSetOf: newOperand)
			addRangeFrom: anInstruction
			to: anInstruction nextInstruction ].

	^ super
		  computeLivenessOfInstruction: anInstruction
		  inBlock: b
		  withLiveSet: live
]

{ #category : #initialization }
DRCogitLinearScanRegisterAllocator >> initialize [

	super initialize.
	self integerRegisters: { 
		DRPhysicalGeneralPurposeRegister name: 'TempReg'.
		DRPhysicalGeneralPurposeRegister name: 'ClassReg'.
		DRPhysicalGeneralPurposeRegister name: 'SendNumArgsReg'.
		DRPhysicalGeneralPurposeRegister name: 'Extra0Reg'.
		DRPhysicalGeneralPurposeRegister name: 'Extra1Reg'.
		DRPhysicalGeneralPurposeRegister name: 'Extra2Reg' }.
	self floatRegisters: { 
		DRPhysicalFloatingPointRegister name: 'DPFPReg0'.
		DRPhysicalFloatingPointRegister name: 'DPFPReg1'.
		DRPhysicalFloatingPointRegister name: 'DPFPReg2'.
		DRPhysicalFloatingPointRegister name: 'DPFPReg3'.
		DRPhysicalFloatingPointRegister name: 'DPFPReg4'. }.
	self spillRegisters: { 
		DRPhysicalGeneralPurposeRegister name: 'Extra3Reg'.
		DRPhysicalGeneralPurposeRegister name: 'Extra4Reg' }
]

{ #category : #'live-analysis' }
DRCogitLinearScanRegisterAllocator >> updateLiveSet: liveSet ofOperand: op ofInstruction: anInstruction inBlock: b [

	"If the instruction is an integer division or modulo, this will be translated in cogit's DIVR:R:Quo:Rem: instruction.
	This instruction requires 4 different registers and not three (operands + destination).
	We model this by adding an extra operand at this point."

	"We need to ignore the last operand"

	((anInstruction isDivision or: [ anInstruction isModulo ]) and: [ 
		 anInstruction type isFloatType not and: [ 
			 op = anInstruction operands last ] ]) ifTrue: [ ^ self ].

	^ super
		  updateLiveSet: liveSet
		  ofOperand: op
		  ofInstruction: anInstruction
		  inBlock: b
]
