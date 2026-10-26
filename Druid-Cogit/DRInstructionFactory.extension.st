Extension { #name : 'DRInstructionFactory' }

{ #category : '*Druid-Cogit' }
DRInstructionFactory >> flushStack [

	^ self flushStackExceptTop: 0
]

{ #category : '*Druid-Cogit' }
DRInstructionFactory >> flushStackExceptTop: n [

	^ DRFlushStack operands: { n } result: DRNoRegister new
]

{ #category : '*Druid-Cogit' }
DRInstructionFactory >> loadArgument: index [

	^ DRCGLoadArgument
		  operands: { index asDRValue }
		  result: self allocateTemporaryRegister
]
