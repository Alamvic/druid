Extension { #name : 'RBMethodNode' }

{ #category : '*Druid' }
RBMethodNode >> allPragmasNamed: aSymbol [ 
	^ pragmas select: [ :pragma| pragma selector = aSymbol ]
]
