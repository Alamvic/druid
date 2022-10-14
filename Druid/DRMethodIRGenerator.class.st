Class {
	#name : #DRMethodIRGenerator,
	#superclass : #DRIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #inline }
DRMethodIRGenerator >> inlineGenerator [

	^ DRMethodIRGeneratorInline new
		  controlFlowGraph: controlFlowGraph;
		  yourself
]

{ #category : #accessing }
DRMethodIRGenerator >> numberOfArguments: aValue [ 
	
	super numberOfArguments: aValue.
	self flag: #TOTHINK. "Push receiver?"
	0 to: numberOfArguments - 1 do: [ :i |
		self push: (self currentBasicBlock loadArgument: i) ]
]

{ #category : #visiting }
DRMethodIRGenerator >> resolveMessageSend: aRBMessageNode receiver: receiver arguments: arguments [
	^ self messageSendInstructionFor: aRBMessageNode receiver: receiver arguments: arguments method: nil
]

{ #category : #visiting }
DRMethodIRGenerator >> visitInstanceVariableNode: aRBVariableNode [

	self
		addInstructionFrom: aRBVariableNode
		instructionKind: DRLoadInstVar
		operands: { aRBVariableNode name asDRValue }
]
