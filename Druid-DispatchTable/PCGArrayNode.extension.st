Extension { #name : #PCGArrayNode }

{ #category : #'*Druid-DispatchTable' }
PCGArrayNode >> maxCompiledPrimitives [
	" Answers the <Number> of 'definitions' (elements) that specify a primitive in the Array of primitives table. 
	This is the parameter of the 'from:' in #initializePrimitiveTable"

	^ self statements size
]