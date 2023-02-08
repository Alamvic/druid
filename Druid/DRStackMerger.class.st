Class {
	#name : #DRStackMerger,
	#superclass : #Object,
	#instVars : [
		'mergedState',
		'mergeBlock',
		'builder'
	],
	#category : #'Druid-Tests'
}

{ #category : #merging }
DRStackMerger >> installInstructions [

	mergedState executionStack framesDo: [ :mergeFrame | 
		mergeFrame temporariesDo: [ :e | mergeBlock addInstruction: e ].
		mergeFrame operandStack do: [ :e | mergeBlock addInstruction: e ].
		mergeBlock addInstruction: mergeFrame returnValue ].
	mergedState vmOperandStack do: [ :e | mergeBlock addInstruction: e ].
	self flag: #OnlyPrimitives.
	mergedState primitiveFailureCode operands ifNotEmpty: [ 
		mergeBlock addInstruction: mergedState primitiveFailureCode ]
]
