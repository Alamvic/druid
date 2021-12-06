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
DRInstructionFactory >> copy: aValue [

	^ DRCopy
		  operands: { aValue asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : #factory }
DRInstructionFactory >> equals: operand1 to: operand2 [
	
	^ DREqualsThanComparison
		  operands: { 
				  operand1 asDRValue.
				  operand2 asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : #factory }
DRInstructionFactory >> initialize [

	super initialize.
	nextAvailableRegister := 0
]

{ #category : #factory }
DRInstructionFactory >> jumpIf: aCondition [

	^ DRConditionalJump operands: { aCondition asDRValue }
]

{ #category : #factory }
DRInstructionFactory >> lessOrEquals: operand1 than: operand2 [

	^ DRLessOrEqualsThanComparison
		  operands: { 
				  operand1 asDRValue.
				  operand2 asDRValue }
		  result: self allocateTemporaryRegister
]

{ #category : #factory }
DRInstructionFactory >> phiWith: value1 [

	^ (DRPhiFunction joinedVariables: { value1 asDRValue })
		  result: self allocateTemporaryRegister;
		  yourself
]

{ #category : #factory }
DRInstructionFactory >> phiWith: value1 with: value2 [

	^ (DRPhiFunction joinedVariables: { 
				   value1 asDRValue.
				   value2 asDRValue })
		  result: self allocateTemporaryRegister;
		  yourself
]

{ #category : #factory }
DRInstructionFactory >> setReturnValue: aValue [ 
	
	^ DRCopy
		  operands: { aValue asDRValue }
		  result: (DRPhysicalRegister name: 'ReceiverResultReg')
]
