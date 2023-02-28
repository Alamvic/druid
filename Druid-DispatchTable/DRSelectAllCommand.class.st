Class {
	#name : #DRSelectAllCommand,
	#superclass : #DRDispatchTableMenuCommand,
	#category : #'Druid-DispatchTable-Commands'
}

{ #category : #'accessing - defaults' }
DRSelectAllCommand class >> defaultDescription [

	^ 'Select all entries'
]

{ #category : #'accessing - defaults' }
DRSelectAllCommand class >> defaultIconName [

	^ #checkedBox
]

{ #category : #'accessing - defaults' }
DRSelectAllCommand class >> defaultName [

	^ 'Select All'
]

{ #category : #testing }
DRSelectAllCommand >> canBeExecuted [

	^ self selectedEntries size ~= self entriesTableItems size
]

{ #category : #executing }
DRSelectAllCommand >> execute [
	" Select all entries items"

	self application selectedEntries: self entriesTable items.
	self updateEntries
]
