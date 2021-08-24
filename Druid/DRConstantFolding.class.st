"
This class is an available optimisations for Druid. 

It is called on DRControlFlowGraph. 

It executes folding on the operations when they contain constants, which reduces the number of useful code lines and allows dead code elimination. 

This optimisation is mainly implemented in the instructions classes, because it depends on the type of operation. Each foldable operation has a tryConstantFolding method. 

[Note] : This class is a step in the building of DRSCCP and is not meant to be called on use cases. 
"
Class {
	#name : #DRConstantFolding,
	#superclass : #Object,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRConstantFolding >> applyTo: anIR [
	
	anIR blocksDo: [ :b | b instructionsDo: [ :i | b replaceInstruction: i by: (i tryConstantFolding) ]].


	
]

{ #category : #operations }
DRConstantFolding >> tryConstantFolding [ 

	^ SubclassResponsibility.
]
