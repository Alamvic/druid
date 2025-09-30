Class {
	#name : 'DRReceiverConstraint',
	#superclass : 'Object',
	#instVars : [
		'constraint'
	],
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'instance creation' }
DRReceiverConstraint class >> with: aConstraint [

	^ self new
		  constraint: aConstraint;
		  yourself
]

{ #category : 'adding' }
DRReceiverConstraint >> addToEdge: aDRCPSEdge [

	| loadReceiverInstructions |
	loadReceiverInstructions := aDRCPSEdge source controlFlowGraph
		                            instructions select: [ :e |
		                            e isLoadReceiver ].
	loadReceiverInstructions do: [ :instruction |
		aDRCPSEdge addConstraint: constraint forInstruction: instruction ]
]

{ #category : 'accessing' }
DRReceiverConstraint >> constraint [
	^ constraint
]

{ #category : 'accessing' }
DRReceiverConstraint >> constraint: aDREqualsConstraint [ 
	constraint := aDREqualsConstraint
]
