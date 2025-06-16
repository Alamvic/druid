Class {
	#name : 'DRIRPrinter',
	#superclass : 'Object',
	#instVars : [
		'cfg'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'instance creation' }
DRIRPrinter class >> for: aDRControlFlowGraph [

	^ self new initializeFor: aDRControlFlowGraph.
]

{ #category : 'converting' }
DRIRPrinter >> asString [

	^ '[[b' , cfg initialBasicBlock id asString , ']]' , String cr , String cr
	  , (cfg blocks flatCollect: [ :b | self blockAsString: b ] as: String)
]

{ #category : 'as yet unclassified' }
DRIRPrinter >> blockAsString: aDRBasicBlock [

	| res |
	res := self blockHeaderFor: aDRBasicBlock.

	res := res , (aDRBasicBlock instructions flatCollect: [ :i |
		        String tab , i printIR , String cr ]).

	^ res
]

{ #category : 'converting' }
DRIRPrinter >> blockHeaderFor: aDRBasicBlock [

	| res |
	res := 'b' , aDRBasicBlock id asString , ':'.

	aDRBasicBlock predecessors size > 1 ifTrue: [
		res := res , ('[' , (', ' join:
			         (aDRBasicBlock predecessors collect: [ :b |
				          'b' , b id asString ])) , ']') ].

	res := res , String cr.
	^ res
]

{ #category : 'initialization' }
DRIRPrinter >> initializeFor: aDRControlFlowGraph [

	cfg := aDRControlFlowGraph.
]
