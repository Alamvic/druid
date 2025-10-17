Class {
	#name : 'DRCogitFormatter',
	#superclass : 'EFFormatter',
	#category : 'Druid-Cogit-CodeGeneration',
	#package : 'Druid-Cogit',
	#tag : 'CodeGeneration'
}

{ #category : 'visiting' }
DRCogitFormatter >> visitAssignmentNode: assignmentNode [

	(assignmentNode value isMessage and: [ 
		 assignmentNode value selector = 'Label' ])
			ifTrue: [ self newLine ].
	super visitAssignmentNode: assignmentNode
]
