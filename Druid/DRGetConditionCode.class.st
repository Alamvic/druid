Class {
	#name : 'DRGetConditionCode',
	#superclass : 'DRInstruction',
	#instVars : [
		'condition'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'testing' }
DRGetConditionCode class >> acceptsOpcode: anOpcode [

	^ DRCondition allSubclasses anySatisfy: [ :class |
		  class ~= DRTypeOf and: [ class new opcode = anOpcode ] ]
]

{ #category : 'testing' }
DRGetConditionCode class >> getDependenciesFromOperandsString: operandsStr withOpcode: opcode [

	| operands conditionClasses condition |
	operands := operandsStr substrings: ','.

	operands := self processDependencyOperands: operands.

	conditionClasses := DRCondition allSubclasses.
	condition := (conditionClasses at:
		              (conditionClasses findFirst: [ :cl |
			               cl ~= DRTypeOf and: [ cl new opcode = opcode ] ]))
		             new.

	^ {
		  ({ condition } , operands).
		  nil }
]

{ #category : 'visiting' }
DRGetConditionCode >> acceptVisitor: aVisitor [
	
	^ aVisitor visitGetConditionCode: self
]

{ #category : 'accessing' }
DRGetConditionCode >> condition [
	
	^ condition
]

{ #category : 'testing' }
DRGetConditionCode >> isSetConditionCode [
	
	^ true
]

{ #category : 'printing' }
DRGetConditionCode >> opcode [
	
	^ condition opcode
]

{ #category : 'accessing' }
DRGetConditionCode >> operands: operandList [

	condition := operandList first value.
	super operands: operandList allButFirst
]

{ #category : 'printing' }
DRGetConditionCode >> prettyPrint [

	| print |
	print := (self isStaged
		          ifTrue: [ '[STAGED] ' ]
		          ifFalse: [ '' ])
	         ,
	         (result
		          ifNil: [ '' ]
		          ifNotNil: [ result prettyPrint , ' := ' ]).
	^ print , self opcode , ' '
	  , (self condition prettyPrintWithBranch: self)
]

{ #category : 'reversing' }
DRGetConditionCode >> reversed [

	^ self class
		  operands: { condition reversed } , operands reversed
		  result: result
]

{ #category : 'SCCP' }
DRGetConditionCode >> sccpLatticeValueFor: sccp [

	| operand1 operand2 |
	operand1 := sccp latticeValue: self operand1.
	operand2 := sccp latticeValue: self operand2.
	
	^ sccp
		  sccpLatticeValueFor: self condition
		  latticeOperand1: operand1
		  latticeOperand2: operand2
]

{ #category : 'types' }
DRGetConditionCode >> type [
	
	^ DRBooleanType new
]
