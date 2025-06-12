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
	res := 'b' , aDRBasicBlock id asString , ':' , String cr.

	res := res , (aDRBasicBlock instructions flatCollect: [ :i |
		       String tab , i printIR, String cr ]).

	^ res
]

{ #category : 'initialization' }
DRIRPrinter >> initializeFor: aDRControlFlowGraph [

	cfg := aDRControlFlowGraph.
]
