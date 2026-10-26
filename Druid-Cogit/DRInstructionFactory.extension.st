Extension { #name : 'DRInstructionFactory' }

{ #category : '*Druid-Cogit' }
DRInstructionFactory >> floatFromBits: operand1 [

	^ DRGetFloatFromBits
		  operands: { operand1 asDRValue. }
		  result: self allocateTemporaryRegister
]

{ #category : '*Druid-Cogit' }
DRInstructionFactory >> flushStack [

	^ self flushStackExceptTop: 0
]

{ #category : '*Druid-Cogit' }
DRInstructionFactory >> flushStackExceptTop: n [

	^ DRFlushStack operands: { n } result: DRNoRegister new
]

{ #category : '*Druid-Cogit' }
DRInstructionFactory >> intToFloat: operand1 [

	^ DRIntToFloat
		  operands: { operand1 asDRValue. }
		  result: self allocateTemporaryRegister
]

{ #category : '*Druid-Cogit' }
DRInstructionFactory >> jitCompileTimeVariable: aString [ 

	^ DRInterpreterReference reference: aString
]

{ #category : '*Druid-Cogit' }
DRInstructionFactory >> loadArgument: index [

	^ DRCGLoadArgument
		  operands: { index asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : '*Druid-Cogit' }
DRInstructionFactory >> loadCogitReceiver [

	^ DRCGLoadReceiver result: self allocateTemporaryRegister
]

{ #category : '*Druid-Cogit' }
DRInstructionFactory >> loadFramePointer [

	^ DRLoadFramePointer result: self allocateTemporaryRegister
]

{ #category : '*Druid-Cogit' }
DRInstructionFactory >> pushBase: base offset: offset [

	^ DRPush
		  operands: { (DRBaseOffsetMemoryAddress new
				   base: base;
				   offset: offset;
				   yourself) }
		  result: DRNoRegister new
]

{ #category : '*Druid-Cogit' }
DRInstructionFactory >> setReturnValue: aValue [ 
	
	^ DRCopy
		  operands: { aValue asDRValue }
		  result: (DRPhysicalGeneralPurposeRegister name: 'ReceiverResultReg')
]

{ #category : '*Druid-Cogit' }
DRInstructionFactory >> unspill: anInteger [

	^ DRUnspillStackSlot operands: { anInteger } result: DRNoRegister new
]
