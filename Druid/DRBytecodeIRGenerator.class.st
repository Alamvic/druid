Class {
	#name : 'DRBytecodeIRGenerator',
	#superclass : 'DRMetaCompilerIRGenerator',
	#instVars : [
		'jumpOffset'
	],
	#category : 'Druid-CompilerCompiler',
	#package : 'Druid',
	#tag : 'CompilerCompiler'
}

{ #category : 'visiting' }
DRBytecodeIRGenerator >> visitClassVariableNode: aRBVariableNode [

	| name value mappings |
	name := aRBVariableNode variable name.
	mappings := {
		            ('FoxIFReceiver'
		             ->
			             ((DRInterpreterReference reference:
					               'FrameReceiverOffset')
				              type: (DRUnsignedIntegerType size: 8);
				              yourself)).
		            ('IMMUTABILITY' -> true asDRValue) } asDictionary.
	
	value := mappings at: name ifAbsent: [ | variableValue variableType |
			variableValue := aRBVariableNode variable value.
			variableType := variableValue isBooleanValue
				ifTrue: [ DRBooleanType new ]
				ifFalse: [ (DRUnsignedIntegerType size: 8) ].
	         (DRInterpreterReference reference: name)
		         type: variableType;
		         yourself ].
	^ self
		  addInstructionFrom: aRBVariableNode
		  instructionKind: DRCopy
		  operands: { value }
]
