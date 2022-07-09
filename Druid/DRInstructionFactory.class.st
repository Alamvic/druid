Class {
	#name : #DRInstructionFactory,
	#superclass : #Object,
	#instVars : [
		'nextAvailableRegister'
	],
	#category : #'Druid-IR'
}

{ #category : #factory }
DRInstructionFactory >> add: operand1 to: operand2 [

	^ DRAdd
		  operands: { operand1 asDRValue. operand2 asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : #factory }
DRInstructionFactory >> allocateTemporaryRegister [
	
	| allocatedRegisterId |
	allocatedRegisterId := nextAvailableRegister.
	nextAvailableRegister := nextAvailableRegister + 1.
	^ DRTemporaryRegister id: allocatedRegisterId
]

{ #category : #factory }
DRInstructionFactory >> and: operand1 with: operand2 [

	^ DRAnd
		  operands: { operand1 asDRValue. operand2 asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : #factory }
DRInstructionFactory >> anyMask: operand1 with: operand2 [

	^ self getConditionCodeOf: DRTestMask new operands: { 
			operand1 asDRValue.
			operand2 asDRValue }
]

{ #category : #factory }
DRInstructionFactory >> bitAnd: operand1 with: operand2 [

	^ DRBitAnd
		  operands: { operand1 asDRValue. operand2 asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : #factory }
DRInstructionFactory >> copy: aValue [

	^ DRCopy
		  operands: { aValue asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : #factory }
DRInstructionFactory >> divide: aValue by: anotherValue [

	^ DRDivision
		  operands: { aValue asDRValue. anotherValue asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : #factory }
DRInstructionFactory >> equals: operand1 to: operand2 [

	^ self getConditionCodeOf: DREqualsThanComparison new operands: { 
			  operand1 asDRValue.
			  operand2 asDRValue }
]

{ #category : #building }
DRInstructionFactory >> getConditionCodeOf: aCondition operands: aCollection [ 

	^ DRGetConditionCode new
		operands: { aCondition }, aCollection;
		result: self allocateTemporaryRegister;
		yourself
]

{ #category : #factory }
DRInstructionFactory >> greater: operand1 than: operand2 [

	^ self getConditionCodeOf: DRGreaterThanComparison new operands: { 
			operand1 asDRValue.
			operand2 asDRValue }
]

{ #category : #factory }
DRInstructionFactory >> initialize [

	super initialize.
	nextAvailableRegister := 0
]

{ #category : #factory }
DRInstructionFactory >> intToFloat: operand1 [

	^ DRIntToFloat
		  operands: { operand1 asDRValue. }
		  result: self allocateTemporaryRegister
]

{ #category : #factory }
DRInstructionFactory >> jumpIf: aBoolean [

	^ DRBranchIfTrue operands: { aBoolean asDRValue }
]

{ #category : #factory }
DRInstructionFactory >> lessOrEquals: operand1 than: operand2 [

	^ self getConditionCodeOf: DRLessOrEqualsThanComparison new operands: { 
			  operand1 asDRValue.
			  operand2 asDRValue }
]

{ #category : #factory }
DRInstructionFactory >> load: anAdress [

	^ DRLoad
		  operands: { 
				  8 asDRValue. "Assume 64 bits for now"
				  anAdress asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : #factory }
DRInstructionFactory >> modulo: aValue by: anotherValue [

	^ DRMod
		  operands: { aValue asDRValue. anotherValue asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : #factory }
DRInstructionFactory >> multiply: aValue by: anotherValue [

	^ DRMultiply
		  operands: { aValue asDRValue. anotherValue asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : #factory }
DRInstructionFactory >> or: operand1 with: operand2 [

	^ DROr
		  operands: { operand1 asDRValue. operand2 asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : #factory }
DRInstructionFactory >> phiWith: value1 [

	^ self phiWithVariables: { value1 asDRValue }
]

{ #category : #factory }
DRInstructionFactory >> phiWith: value1 with: value2 [

	^ self phiWithVariables: { 
			  value1 asDRValue.
			  value2 asDRValue }
]

{ #category : #factory }
DRInstructionFactory >> phiWithVariables: variables [

	^ (DRPhiFunction joinedVariables: variables)
		  result: self allocateTemporaryRegister;
		  yourself
]

{ #category : #factory }
DRInstructionFactory >> setReturnValue: aValue [ 
	
	^ DRCopy
		  operands: { aValue asDRValue }
		  result: (DRPhysicalRegister name: 'ReceiverResultReg')
]
