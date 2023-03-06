"
When executed, this command open a class hierarchy browser on the selected entry.
"
Class {
	#name : #DRBrowseMethodCommand,
	#superclass : #DRDispatchTableMenuCommand,
	#category : #'Druid-DispatchTable-Commands'
}

{ #category : #'accessing - defaults' }
DRBrowseMethodCommand class >> defaultDescription [

	^ 'Browse entry'
]

{ #category : #'accessing - defaults' }
DRBrowseMethodCommand class >> defaultIconName [

	^ #glamorousBrowse
]

{ #category : #'accessing - defaults' }
DRBrowseMethodCommand class >> defaultName [

	^ 'Browse'
]

{ #category : #testing }
DRBrowseMethodCommand >> canBeExecuted [

	^ self entriesTableItems notEmpty
]

{ #category : #executing }
DRBrowseMethodCommand >> execute [
	"Private - Open a Browser on the selected method"

	self stBrowser
		openOnClass: self selectedEntry methodClass
		selector: self selectedEntry selector


]
