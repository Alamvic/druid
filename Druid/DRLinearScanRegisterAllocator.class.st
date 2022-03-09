Class {
	#name : #DRLinearScanRegisterAllocator,
	#superclass : #Object,
	#instVars : [
		'blocks',
		'liveSets',
		'availableRegisters'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #allocation }
DRLinearScanRegisterAllocator >> allocateRegisterForLiveRegisterInterval: aDRLiveRegisterInterval [
	
	| register |
	register := availableRegisters removeFirst.
	aDRLiveRegisterInterval register: register.
]

{ #category : #allocation }
DRLinearScanRegisterAllocator >> allocateRegisters [
	
	| activeIntervals |
	activeIntervals := Set new.
	liveSets values do: [ :currentInterval |
		
		activeIntervals copy do: [ :eachInterval |
			(eachInterval lastUsage isNil or:
				[ eachInterval lastUsage order <= currentInterval definition order ])
					ifTrue: [ 
						"not active anymore"
						activeIntervals remove: eachInterval.
						self returnRegister: eachInterval register ] ].
		
		self allocateRegisterForLiveRegisterInterval: currentInterval.
		currentInterval isSpilled ifFalse: [ activeIntervals add: currentInterval ].
	]
]

{ #category : #api }
DRLinearScanRegisterAllocator >> allocateRegistersIn: aDRControlFlowGraph [ 

	self linearizeBlocks: aDRControlFlowGraph.
	self buildLiveIntervals.
	self allocateRegisters.
	self assignRegisters.
]

{ #category : #assigning }
DRLinearScanRegisterAllocator >> assignRegisters [
	
	self instructionsDo: [ :i |
		i result ifNotNil: [
			i result isVirtualRegister ifTrue: [ 
				i result: (liveSets at: i result) register ].
			i operands: (i operands
				collect: [ :operand |
					operand result isVirtualRegister
						ifTrue: [ (liveSets at: operand result result) register ]
						ifFalse: [ operand ] ]) ] ]
]

{ #category : #'live-analysis' }
DRLinearScanRegisterAllocator >> buildLiveIntervals [
	
	"For now do a simple live interval computation.
	This could be further improved with range splitting and other optimizations"
	self computeLiveSets.
]

{ #category : #'live-analysis' }
DRLinearScanRegisterAllocator >> computeLiveSets [
	
	self instructionsDo: [ :i |
		"If the instruction has a result..."
		i result ifNotNil: [ | liveSet |
			liveSet := DRLiveSet new.
			liveSet definition: i.
			i result isVirtualRegister ifTrue: [
				liveSets at: i result put: liveSet ].
			i operands
				select: [ :op | op result isVirtualRegister ]
				thenDo: [ :reg |
					(liveSets at: reg result) lastUsage: i ] ] ].
]

{ #category : #'block-linearization' }
DRLinearScanRegisterAllocator >> initialize [

	super initialize.
	liveSets := OrderedDictionary new.
	self registers: { 
		DRPhysicalRegister name: 'TempReg'.
		DRPhysicalRegister name: 'ClassReg'.
		DRPhysicalRegister name: 'SendNumArgsReg'.
		DRPhysicalRegister name: 'Extra0Reg'.
		DRPhysicalRegister name: 'Extra1Reg'.
		DRPhysicalRegister name: 'Extra2Reg'.
		DRPhysicalRegister name: 'Extra3Reg'.
		DRPhysicalRegister name: 'Extra4Reg' }
]

{ #category : #enumerating }
DRLinearScanRegisterAllocator >> instructionsDo: aFullBlockClosure [ 
	
	blocks do: [ :b |
		b instructionsDo: aFullBlockClosure ]
]

{ #category : #'block-linearization' }
DRLinearScanRegisterAllocator >> linearizeBlocks: aDRControlFlowGraph [ 
	
	| order |
	blocks := aDRControlFlowGraph reversePostOrderBlocks.
	
	order := 1.
	self instructionsDo: [ :i | 
		i order: order.
		order := order + 1 ]
]

{ #category : #'block-linearization' }
DRLinearScanRegisterAllocator >> registers: aCollection [

	availableRegisters := aCollection asOrderedCollection
]

{ #category : #allocation }
DRLinearScanRegisterAllocator >> returnRegister: aPhisicalRegister [

	availableRegisters addLast: aPhisicalRegister
]
