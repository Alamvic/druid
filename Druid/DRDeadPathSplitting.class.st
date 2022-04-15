Class {
	#name : #DRDeadPathSplitting,
	#superclass : #DRControlFlowOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRDeadPathSplitting >> doApply: aCFG [

	aCFG generatePaths.
	(aCFG deadPaths keys 
		collect: [ :edge | edge source -> edge destination ] as: Set)
		do: [ :association |
			association value splitTo: association key ]
		
]
