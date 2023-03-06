"
When executed, this command will unselect previously selected entries in the UI.
"
Class {
	#name : #DRSelectNoneCommand,
	#superclass : #DRDispatchTableMenuCommand,
	#category : #'Druid-DispatchTable-Commands'
}

{ #category : #'accessing - defaults' }
DRSelectNoneCommand class >> defaultDescription [

	^ 'Unselect all entries'
]

{ #category : #'accessing - defaults' }
DRSelectNoneCommand class >> defaultIconName [

	^ #checkboxUnselected
]

{ #category : #'accessing - defaults' }
DRSelectNoneCommand class >> defaultName [

	^ 'Select None'
]

{ #category : #testing }
DRSelectNoneCommand >> canBeExecuted [ 

	^ self entriesTableItems notEmpty
]

{ #category : #executing }
DRSelectNoneCommand >> execute [
	" Select all entries items"

	self application selectedEntries: Array empty.
	self updateEntries
]
