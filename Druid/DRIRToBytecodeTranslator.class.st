Class {
	#name : 'DRIRToBytecodeTranslator',
	#superclass : 'Object',
	#instVars : [
		'gen',
		'allTemps',
		'vectorTemps',
		'compilationContext'
	],
	#category : 'Druid-BytecodeGeneration',
	#package : 'Druid',
	#tag : 'BytecodeGeneration'
}

{ #category : 'accessing' }
DRIRToBytecodeTranslator >> compilationContext: aContext [

	compilationContext := aContext.
	gen compilationContext: aContext
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> compileBlockIR: anIR withScope: scope [

	gen inBlock: true.
	
	allTemps := scope allLocalTemps .
	vectorTemps := scope allVectorTemp.
	
	self visitNode: anIR.
	
	^ gen compiledBlock.
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> compileIR: anIR withScope: scope [

	 
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> indexForVar: aName [

	^ allTemps indexOf: aName.
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> initialize [ 

	gen := IRBytecodeGenerator newWithEncoderClass: EncoderForSistaV1.
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> vectorForVar: aName [

	^ vectorTemps at: aName.
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitBlockReturnTop: blockReturnTop [

	gen blockReturnTop
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitInstruction: anInstr [

	gen mapBytesTo: anInstr.
	self visitNode: anInstr.
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitJump: jump [

	gen goto: jump destination orderNumber
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitJumpIf: jumpIf [

	gen
		if: jumpIf boolean
		goto: jumpIf destination orderNumber
		otherwise: jumpIf otherwise orderNumber
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitMethod: anIR [

	IRFix new visitNode: anIR.

	"need to check what all these do"
	gen irPrimitive: anIR irPrimitive.
	gen numArgs: anIR numArgs.
	gen properties: anIR properties.
	gen numTemps: anIR tempMap size.

	self visitSequences: anIR allSequences.

	anIR additionalLiterals do: [ :each | gen addLiteral: each ]
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitNode: elem [

	^ elem accept: self
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitNodes: col [

	col do: [ :each | self visitNode: each ]
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitPop: pop [

	gen popTop
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitPopIntoRemoteTemp: remoteTemp [

	| tempVectorVars tempVectorIndex tempIndex |
	tempVectorVars := self vectorForVar: remoteTemp tempVectorName.
	tempVectorIndex := self indexForVar: remoteTemp tempVectorName.
	tempIndex := tempVectorVars indexOf: remoteTemp name. 
	
	gen storePopRemoteTemp: tempIndex inVectorAt: tempVectorIndex
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitPushFullClosure: fullBlock [
	
	fullBlock copiedValues do: [:name |
		gen pushTemp: (self indexForVar: name)].
	gen mapBytesTo: fullBlock.
	gen pushFullBlockClosure: fullBlock
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitPushLiteral: lit [

	^ gen pushLiteral: lit literal beReadOnlyLiteral
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitPushReceiver: receiver [

	gen pushReceiver
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitPushRemoteTemp: remoteTemp [

	| tempVectorVars tempVectorIndex tempIndex |
	
	tempVectorVars := self vectorForVar: remoteTemp tempVectorName.
	tempVectorIndex := self indexForVar: remoteTemp tempVectorName.
	tempIndex := tempVectorVars indexOf: remoteTemp name. 
	
	gen pushRemoteTemp: tempIndex inVectorAt: tempVectorIndex
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitPushTemp: tmp [

	gen pushTemp: (self indexForVar: tmp name)
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitReturn: ret [

	gen returnTop
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitReturnLiteral: lit [

	gen returnConstant: lit literal
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitSend: send [
	
	send superOf
		ifNil: [ gen send: send selector ]
		ifNotNil: [ :behavior |  gen send: send selector toSuperOf: behavior ]
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitSequence: sequence [

	gen label: sequence orderNumber.
	
	sequence do: [ :instr | self visitInstruction: instr ]
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitSequences:  irSequences [

	irSequences withIndexDo: [ :seq :i | seq orderNumber: i].
	self visitNodes: irSequences
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitStoreInstVar: instVar [
	gen storeInstVar: instVar index
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitStoreRemoteTemp: remoteTemp [

	| tempVectorVars tempVectorIndex tempIndex |
	
	tempVectorVars := self vectorForVar: remoteTemp tempVectorName.
	tempVectorIndex := self indexForVar: remoteTemp tempVectorName.
	tempIndex := tempVectorVars indexOf: remoteTemp name. 
	
	gen storeRemoteTemp: tempIndex inVectorAt: tempVectorIndex
]

{ #category : 'initialization' }
DRIRToBytecodeTranslator >> visitTempVector: tempVector [

	gen pushNewArray: tempVector vars size.
	gen storePopTemp: (self indexForVar: tempVector name)
]
