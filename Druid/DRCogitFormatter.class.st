Class {
	#name : 'DRCogitFormatter',
	#superclass : 'EFFormatter',
	#category : 'Druid-Cogit',
	#package : 'Druid',
	#tag : 'Cogit'
}

{ #category : 'visiting' }
DRCogitFormatter >> visitAssignmentNode: assignmentNode [

	(assignmentNode value isMessage and: [ 
		 assignmentNode value selector = 'Label' ])
			ifTrue: [ self newLine ].
	super visitAssignmentNode: assignmentNode
]
