Class {
	#name : 'DRCogitCodeGenerator',
	#superclass : 'Object',
	#instVars : [
		'generatorMethodBuilder',
		'fallthroughBlock',
		'deferredBranches',
		'availableJumpIndexes',
		'blocks',
		'visitedBlocksWithIncomingBackjumps',
		'visitedBlocks',
		'currentBlock'
	],
	#category : 'Druid-Cogit',
	#package : 'Druid',
	#tag : 'Cogit'
}

{ #category : 'visiting' }
DRCogitCodeGenerator >> visitSignedCoercion: aDRCogitUnsignedCoercion [ 

	^ self visitCopy: aDRCogitUnsignedCoercion
]
