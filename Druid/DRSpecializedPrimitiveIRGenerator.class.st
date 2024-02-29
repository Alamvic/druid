Class {
	#name : #DRSpecializedPrimitiveIRGenerator,
	#superclass : #DRPrimitiveIRGenerator,
	#instVars : [
		'specializedFormat'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #specialization }
DRSpecializedPrimitiveIRGenerator >> interpretFormatOfHeaderWith: aRBMessageNode [

	| headerInstruction loadInstruction obj |
	headerInstruction := self visitOperand: aRBMessageNode arguments first.
	loadInstruction := headerInstruction simpleConstantFold.

	self assert: loadInstruction isLoad.

	obj := loadInstruction address expression simpleConstantFold.

	obj isLoadReceiver ifFalse: [ 1 halt "?" ].

	self pushOperand: specializedFormat asDRValue
]

{ #category : #specialization }
DRSpecializedPrimitiveIRGenerator >> specializeForReceiverFormat: aFormat [

	specializedFormat := aFormat.
	specialCases at: #formatOfHeader: put: #interpretFormatOfHeaderWith:
]
