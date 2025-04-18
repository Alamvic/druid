Class {
	#name : 'DRInstructionFactory',
	#superclass : 'Object',
	#instVars : [
		'nextAvailableRegister'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'factory' }
DRInstructionFactory >> add: operand1 to: operand2 [

	^ DRAdd
		  operands: { operand1 asDRValue. operand2 asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> allocateTemporaryRegister [
	
	| allocatedRegisterId |
	allocatedRegisterId := nextAvailableRegister.
	nextAvailableRegister := nextAvailableRegister + 1.
	^ DRSSARegister name: allocatedRegisterId
]

{ #category : 'factory' }
DRInstructionFactory >> and: operand1 with: operand2 [

	^ DRAnd
		  operands: { operand1 asDRValue. operand2 asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> anyMask: operand1 with: operand2 [

	^ self getConditionCodeOf: DRTestMask new operands: { 
			operand1 asDRValue.
			operand2 asDRValue }
]

{ #category : 'factory' }
DRInstructionFactory >> bitAnd: operand1 with: operand2 [

	^ DRBitAnd
		  operands: { operand1 asDRValue. operand2 asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> bitOr: operand1 with: operand2 [

	^ DRBitOr
		  operands: { operand1 asDRValue. operand2 asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> bitXor: operand1 with: operand2 [

	^ DRBitXor
		  operands: { operand1 asDRValue. operand2 asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> copy: aValue [

	^ DRCopy
		  operands: { aValue asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> divide: aValue by: anotherValue [

	^ DRDivision
		  operands: { aValue asDRValue. anotherValue asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> equals: operand1 to: operand2 [

	^ self getConditionCodeOf: DREqualsThanComparison new operands: { 
			  operand1 asDRValue.
			  operand2 asDRValue }
]

{ #category : 'factory' }
DRInstructionFactory >> floatFromBits: operand1 [

	^ DRGetFloatFromBits
		  operands: { operand1 asDRValue. }
		  result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> flushStack [

	^ self flushStackExceptTop: 0
]

{ #category : 'factory' }
DRInstructionFactory >> flushStackExceptTop: n [

	^ DRFlushStack operands: { n } result: DRNoRegister new
]

{ #category : 'factory' }
DRInstructionFactory >> frameReturn: aValue [

	^ DRFrameReturn operands: { aValue asDRValue } result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> getConditionCodeOf: aCondition operands: aCollection [ 

	^ DRGetConditionCode new
		operands: { aCondition }, aCollection;
		result: self allocateTemporaryRegister;
		yourself
]

{ #category : 'factory' }
DRInstructionFactory >> greater: operand1 than: operand2 [

	^ self getConditionCodeOf: DRGreaterThanComparison new operands: { 
			operand1 asDRValue.
			operand2 asDRValue }
]

{ #category : 'factory' }
DRInstructionFactory >> initialize [

	super initialize.
	nextAvailableRegister := 0
]

{ #category : 'factory' }
DRInstructionFactory >> intToFloat: operand1 [

	^ DRIntToFloat
		  operands: { operand1 asDRValue. }
		  result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> jitCompileTimeExpression: aString [

	^ DRInterpreterReference reference: aString
]

{ #category : 'factory' }
DRInstructionFactory >> jitCompileTimeVariable: aString [ 

	^ DRInterpreterReference reference: aString
]

{ #category : 'factory' }
DRInstructionFactory >> jumpIf: aBoolean [

	^ DRBranchIfCondition operands: {
			  DREqualsThanComparison new.
			  aBoolean asDRValue.
			  true asDRValue }
]

{ #category : 'factory' }
DRInstructionFactory >> jumpIf: aCondition trueTarget: trueTarget falseTarget: falseTarget [

	^ (self jumpIf: aCondition)
		  trueBranch: trueTarget;
		  falseBranch: falseTarget;
		  yourself
]

{ #category : 'factory' }
DRInstructionFactory >> lessOrEquals: operand1 than: operand2 [

	^ self getConditionCodeOf: DRLessOrEqualsThanComparison new operands: { 
			  operand1 asDRValue.
			  operand2 asDRValue }
]

{ #category : 'factory' }
DRInstructionFactory >> load: anAdress [

	^ self loadSInt64: anAdress
]

{ #category : 'factory' }
DRInstructionFactory >> loadArgument: index [

	^ DRLoadArgument
		  operands: { index asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> loadFramePointer [

	^ DRLoadFramePointer result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> loadReceiver [

	^ DRLoadReceiver result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> loadSInt64: operand [

	^ DRLoad
		  operands: { 
				  DRSignedIntegerType size: 8.
				  operand asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : 'building' }
DRInstructionFactory >> loadStackTop [

	^ self loadStackValueAt: 0
]

{ #category : 'accessing' }
DRInstructionFactory >> loadStackValueAt: anInteger [

	^ DRLoadStackValue operands: { anInteger asDRValue } result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> messageSend: selector withOperands: operands [

	^ DRMessageSend
		  operands: operands
		  result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> modulo: aValue by: anotherValue [

	^ DRMod
		  operands: { aValue asDRValue. anotherValue asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> multiply: aValue by: anotherValue [

	^ DRMultiply
		  operands: { aValue asDRValue. anotherValue asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> notEquals: operand1 to: operand2 [

	^ self getConditionCodeOf: DRNotEqualsThanComparison new operands: { 
			  operand1 asDRValue.
			  operand2 asDRValue }
]

{ #category : 'factory' }
DRInstructionFactory >> or: operand1 with: operand2 [

	^ DROr
		  operands: { operand1 asDRValue. operand2 asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> phiWith: value1 [

	^ self phiWithVariables: { value1 asDRValue }
]

{ #category : 'factory' }
DRInstructionFactory >> phiWith: value1 with: value2 [

	^ self phiWithVariables: { 
			  value1 asDRValue.
			  value2 asDRValue }
]

{ #category : 'factory' }
DRInstructionFactory >> phiWithVariables: variables [

	^ (DRPhiFunction joinedVariables: variables)
		  result: self allocateTemporaryRegister;
		  yourself
]

{ #category : 'factory' }
DRInstructionFactory >> piConstraint: aDRConditionalConstraint on: aValue [

	^ DRPiNode constraint: aDRConditionalConstraint
			on: aValue;
			result: self allocateTemporaryRegister;
			yourself
]

{ #category : 'factory' }
DRInstructionFactory >> pop [

	^ self pop: 1 unspilled: false
]

{ #category : 'building' }
DRInstructionFactory >> pop: anInteger unspilled: aBoolean [

	^ DRPop
		  operands: {
				  anInteger.
				  aBoolean }
		  result: DRNoRegister new
]

{ #category : 'factory' }
DRInstructionFactory >> push: operand [

	^ DRPush
		  operands: { operand asDRValue }
		  result: DRNoRegister new
]

{ #category : 'factory' }
DRInstructionFactory >> pushBase: base offset: offset [

	^ DRPush
		  operands: { (DRBaseOffsetMemoryAddress new
				   base: base;
				   offset: offset;
				   yourself) }
		  result: DRNoRegister new
]

{ #category : 'building' }
DRInstructionFactory >> rotateLeft: aValue by: anOffset [

	^ DRRotateLeft
		  operands: {
				  aValue asDRValue.
				  anOffset asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> setReturnValue: aValue [ 
	
	^ DRCopy
		  operands: { aValue asDRValue }
		  result: (DRPhysicalGeneralPurposeRegister name: 'ReceiverResultReg')
]

{ #category : 'factory' }
DRInstructionFactory >> storeSInt64: operand at: address [

	^ DRStore
		  operands: {
				  8 asDRValue.
				  operand asDRValue.
				  address asDRValue }
		  result: DRNoRegister new
]

{ #category : 'factory' }
DRInstructionFactory >> strongCopyForInlining: aValue [

	^ DRStrongCopyForInlining
		  operands: { aValue asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> sub: operand1 from: operand2 [

	^ DRSubtract 
		  operands: { operand1 asDRValue. operand2 asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : 'factory' }
DRInstructionFactory >> subtract: subtrahend from: minuend [

	^ DRSubtract
		  operands: { minuend asDRValue. subtrahend asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : 'building' }
DRInstructionFactory >> unspill: anInteger [

	^ DRUnspillStackSlot operands: { anInteger } result: DRNoRegister new
]
