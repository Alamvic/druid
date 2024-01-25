Extension { #name : #PCGArrayNode }

{ #category : #'*Druid' }
PCGArrayNode >> maxCompiledPrimitives [
	" Answers the <Number> of 'definitions' (elements) that specify a primitive in the Array of primitives table. 
	This is the parameter of the 'from:' in #initializePrimitiveTable"

	self statements ifEmpty: [ ^ 0 ].
	^ self statements last statements first
]
