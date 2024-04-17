Class {
	#name : #DRCogitStager,
	#superclass : #DRStager,
	#category : #'Druid-Staging'
}

{ #category : #staging }
DRCogitStager >> canStage: anInstruction [

	(anInstruction isCopy and: [anInstruction type isFloatType ])
		ifTrue: [ ^ false ].
	^ super canStage: anInstruction
]
