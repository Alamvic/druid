Extension { #name : 'OCMethodNode' }

{ #category : '*Druid' }
OCMethodNode >> allPragmasNamed: aSymbol [ 
	^ pragmas select: [ :pragma| pragma selector = aSymbol ]
]
