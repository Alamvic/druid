Class {
	#name : 'DRFrameReturn',
	#superclass : 'DRStrongCopyForInlining',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'adding' }
DRFrameReturn >> addNewReturn: aDRInstruction [

	"IR manipulation:
	  - Break the basic block on self
	  - Jump the block of the instrucion (parameter) to me
	  - Replace operand by a phi with old and new values"

	| current phi |
	self assert: aDRInstruction basicBlock endInstruction isNoop.

	self breakBasicBlock.
	aDRInstruction basicBlock jumpTo: self basicBlock.

	current := self operand.
	phi := self basicBlock addInstruction:
		       (self instructionFactory phiWithVariables: {
				        current.
				        aDRInstruction }).
	self replaceOperand: current by: phi
]

{ #category : 'testing' }
DRFrameReturn >> isCopy [

	^ self shouldKeepInCFG not
]

{ #category : 'testing' }
DRFrameReturn >> isFrameReturn [

	^ true
]

{ #category : 'SCCP' }
DRFrameReturn >> sccpLatticeValueFor: sccp [

	self shouldKeepInCFG ifTrue: [ ^ sccp bottom ].

	^ super sccpLatticeValueFor: sccp
]
