Class {
	#name : #DRDeoptimizationInformation,
	#superclass : #Object,
	#instVars : [
		'entries'
	],
	#category : #'Druid-BytecodeGeneration-Deoptimization'
}

{ #category : #adding }
DRDeoptimizationInformation >> addEntryFor: anIRSend forState: aDRExecutionState [

	| entry |
	entry := DRDeoptimizationEntry new
		         irInstruction: anIRSend;
		         yourself.

	aDRExecutionState executionStack framesDo: [ :aFrame | | methodNode |
		methodNode := aFrame method.
		entry 
			addFrameForMethod: (self compiledCodeFor: methodNode)
			savedPC: (methodNode pcsForNode: aFrame currentNode) max + 1
			receiver: aFrame receiver
			arguments: (methodNode argumentNames collect: [ :e | aFrame readTemporaryNamed: e ])
			temporaries: (methodNode temporaryNames collect: [ :e | aFrame readTemporaryNamed: e ])
			stack: aFrame operandStack ].
		
	entries add: entry
]

{ #category : #'as yet unclassified' }
DRDeoptimizationInformation >> compiledCodeFor: aRBMethodOrBlock [ 
	
	^ aRBMethodOrBlock isMethod
		ifTrue: [ aRBMethodOrBlock method ]
		ifFalse: [ aRBMethodOrBlock methodNode method allBlocks detect: [ :e | e ast = aRBMethodOrBlock ] ]
	
]

{ #category : #deoptimization }
DRDeoptimizationInformation >> deoptimizeContext: originalContext [

	^ entries 
		detect: [ :entry | entry pc = originalContext pc ]
		ifFound: [ :entry | entry deoptimizeContext: originalContext ]
		ifNone: [ originalContext ]
]

{ #category : #adding }
DRDeoptimizationInformation >> initialize [

	super initialize.
	entries := OrderedCollection new
]

{ #category : #'as yet unclassified' }
DRDeoptimizationInformation >> updatePCForEntries: aDRBytecodeGenerator [

	entries do: [ :e | e updatePCFrom: aDRBytecodeGenerator ]
]
