Class {
	#name : #DRFailureCodeBasedTailDuplication,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRFailureCodeBasedTailDuplication >> applyTo: aCFG [

	aCFG successExitBlock predecessors size = 1 ifTrue: [ 
		aCFG successExitBlock predecessors anyOne tailDuplicate ]
]
