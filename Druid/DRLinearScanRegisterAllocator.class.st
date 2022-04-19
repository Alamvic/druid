Class {
	#name : #DRLinearScanRegisterAllocator,
	#superclass : #Object,
	#instVars : [
		'blocks',
		'liveSets',
		'availableRegisters',
		'spillRegisters'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #allocation }
DRLinearScanRegisterAllocator >> allocateRegisterForLiveRegisterInterval: aDRLiveRegisterInterval withActiveIntervals: activeIntervals [
	
	| register intervalToSpill |
	
	availableRegisters ifNotEmpty: [ 
		register := availableRegisters removeFirst.
		^ aDRLiveRegisterInterval location: register ].
	
	"If empty, spill an interval"
	intervalToSpill := activeIntervals anyOne.
	activeIntervals remove: intervalToSpill.
	aDRLiveRegisterInterval location: intervalToSpill location.
	intervalToSpill location: self newStackLocation
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
						self returnRegister: eachInterval location ] ].
		
		self allocateRegisterForLiveRegisterInterval: currentInterval withActiveIntervals: activeIntervals.
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

{ #category : #accessing }
DRLinearScanRegisterAllocator >> allocatedRegisterFor: aValue [
	
	^ (liveSets at: aValue result) location
]

{ #category : #assigning }
DRLinearScanRegisterAllocator >> assignRegisterToResultInInstruction: i [

	| allocatedLocation allocatedRegister |
	i result ifNil: [ ^ self ].
	i result isVirtualRegister ifFalse: [ ^ self ].

	allocatedLocation := (liveSets at: i) location.
	allocatedRegister := allocatedLocation isMemoryAddress
		ifTrue: [ spillRegisters first ]
		ifFalse: [ allocatedLocation ].
	
	i result: allocatedRegister.

	allocatedLocation isMemoryAddress ifTrue: [ 
		i addNext: (DRStore
			 operands: { allocatedRegister }
			 result: allocatedLocation)]
]

{ #category : #assigning }
DRLinearScanRegisterAllocator >> assignRegisters [

	self instructionsDo: [ :i | "Assign first registers to operands.
		This will use spill registers if needed and return them to the pool.
		Then, the result can use them if it is spilled too."
		self assignRegistersToOperandsInInstruction: i.

		"Replace the result by the allocated register if necessary"
		self assignRegisterToResultInInstruction: i ]
]

{ #category : #assigning }
DRLinearScanRegisterAllocator >> assignRegistersToOperandsInInstruction: i [

	"Assign first registers to operands.
		This will use spill registers if needed and return them to the pool.
		Then, the result can use them if it is spilled too."

	| allocatedLocation allocatedRegister usedSpillRegisters addressSize |
	usedSpillRegisters := OrderedCollection new: 3.
	addressSize := 8 asDRValue.
	i operands do: [ :op |
		(liveSets includesKey: op) ifTrue: [
			allocatedLocation := (liveSets at: op) location.
			allocatedRegister := allocatedLocation isMemoryAddress
				ifTrue: [ usedSpillRegisters addFirst: spillRegisters removeFirst ]
				ifFalse: [ allocatedLocation ].

			allocatedLocation isMemoryAddress ifTrue: [
				i addBefore: (DRLoad
					operands: { addressSize. allocatedLocation }
					result: allocatedRegister) ] ] ].
	
	usedSpillRegisters do: [ :each | spillRegisters addFirst: each ]
]

{ #category : #'live-analysis' }
DRLinearScanRegisterAllocator >> buildLiveIntervals [
	
	"For now do a simple live interval computation.
	This could be further improved with range splitting and other optimizations"
	self computeLiveSets.
]

{ #category : #'live-analysis' }
DRLinearScanRegisterAllocator >> computeLiveSets [

	self instructionsDo: [ :i | "If the instruction has a result..." 
		self computeLiveSetsForInstruction: i ]
]

{ #category : #'live-analysis' }
DRLinearScanRegisterAllocator >> computeLiveSetsForInstruction: i [

	^ i result ifNotNil: [ 
		  | liveSet |
		  liveSet := DRLiveSet new.
		  liveSet definition: i.
		  i result isVirtualRegister ifTrue: [ 
			  liveSets at: i put: liveSet ].
		  i operands
			  select: [ :op | op result isVirtualRegister ]
			  thenDo: [ :reg | (liveSets at: reg) lastUsage: i ] ] "If the instruction has a result..."
]

{ #category : #'block-linearization' }
DRLinearScanRegisterAllocator >> initialize [

	super initialize.
	liveSets := OrderedIdentityDictionary new.
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

{ #category : #allocation }
DRLinearScanRegisterAllocator >> newStackLocation [
	
	^ DRMemoryAddress new
]

{ #category : #'block-linearization' }
DRLinearScanRegisterAllocator >> registers: aCollection [

	availableRegisters := aCollection asOrderedCollection
]

{ #category : #allocation }
DRLinearScanRegisterAllocator >> returnRegister: aPhisicalRegister [

	availableRegisters addLast: aPhisicalRegister
]

{ #category : #accessing }
DRLinearScanRegisterAllocator >> spillRegisters: aCollection [
	
	spillRegisters := aCollection asOrderedCollection
]
