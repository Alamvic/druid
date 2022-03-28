Class {
	#name : #DRConditionalPathSplitting,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRConditionalPathSplitting >> applyTo: aCFG [

	aCFG generatePaths.
	(aCFG deadPaths keys 
		collect: [ :edge | edge source -> edge destination ] as: Set)
		do: [ :association |
			association value tailDuplicate "Duplicate only to source / key" ]
		
]
