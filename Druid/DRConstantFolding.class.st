"
This class calls constant folding on operations. 
"
Class {
	#name : #DRConstantFolding,
	#superclass : #Object,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRConstantFolding >> applyTo: anIR [
	
	anIR blocksDo: [ :b | b tryConstantFolding ]. 
	
	
	


	
]

{ #category : #operations }
DRConstantFolding >> tryConstantFolding [ 

	^ SubclassResponsibility.
]
