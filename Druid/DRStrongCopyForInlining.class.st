Class {
	#name : #DRStrongCopyForInlining,
	#superclass : #DRCopy,
	#category : #'Druid-IR'
}

{ #category : #converting }
DRStrongCopyForInlining >> asCopy [

	^ DRCopy operands: self operands copy result: self result
]

{ #category : #testing }
DRStrongCopyForInlining >> isMandatoryInstruction [

	^ self shouldKeepInCFG
]

{ #category : #testing }
DRStrongCopyForInlining >> shouldKeepInCFG [

	^ self controlFlowGraph messageSends isNotEmpty
]

{ #category : #SCCP }
DRStrongCopyForInlining >> supportConstantOperand [

	^ self shouldKeepInCFG not
]
