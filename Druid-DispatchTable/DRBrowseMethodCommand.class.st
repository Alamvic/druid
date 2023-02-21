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

{ #category : #executing }
DRBrowseMethodCommand >> execute [
	"Private - Open a Browser on the selected method"

	| selectedEntry |
	
	selectedEntry := self entriesTable selectedItem.
	self stBrowser
		openOnClass: selectedEntry methodClass
		selector: selectedEntry selector


]
