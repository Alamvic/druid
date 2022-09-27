Class {
	#name : #DRLoopUnrolling,
	#superclass : #DROptimisation,
	#instVars : [
		'unrollingFactor'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #'instance creation' }
DRLoopUnrolling class >> withUnrollingFactor: unrollingFactor [

	| instance |
	instance := self new.
	instance unrollingFactor: unrollingFactor.
	^ instance
]

{ #category : #operations }
DRLoopUnrolling >> applyTo: aDRControlFlowGraph [

	"The current implementation of loop unrolling only optimizes loops that contain a single basic block (i.e. loops with straight-line code)"

	aDRControlFlowGraph allLoops do: [ :loop | 
		loop unroll: unrollingFactor ]
]

{ #category : #initialization }
DRLoopUnrolling >> initialize [

	unrollingFactor := 4.
]

{ #category : #accessing }
DRLoopUnrolling >> unrollingFactor: anUnrollingFactor [ 
	unrollingFactor := anUnrollingFactor
]
