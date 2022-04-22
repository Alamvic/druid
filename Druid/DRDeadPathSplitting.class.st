Class {
	#name : #DRDeadPathSplitting,
	#superclass : #DRControlFlowOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRDeadPathSplitting >> doApply: aCFG [

	| edge |
	aCFG generatePaths.
	edge := aCFG deadPaths keys first.
	edge destination splitTo: edge source.
	aCFG validate.
	aCFG generatePaths

	"	(aCFG deadPaths keys 
		collect: [ :edge | edge source -> edge destination ] as: Set)
		do: [ :association |
			association value splitTo: association key ]
	"
]
