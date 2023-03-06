Class {
	#name : #DRDispatchTableApp,
	#superclass : #SpApplication,
	#category : #'Druid-DispatchTable-UI'
}

{ #category : #examples }
DRDispatchTableApp class >> example1 [

	DRDispatchTableApp new start
]

{ #category : #'as yet unclassified' }
DRDispatchTableApp class >> menuDispatchTableOn: aBuilder [
	<worldMenu>

	(aBuilder item: #DTB)
		parent: #Tools;
		label: 'Dispatch Table';
		icon: (self iconNamed: #catalog);
		action: [ DRDispatchTableApp new run ];
		help: 'Dispatch Table Builder';
		order: 630
]

{ #category : #accessing }
DRDispatchTableApp >> selectedEntries [
	"Answer a <Collection> with the entries marked for installation"

	^ self
		propertyAt: #selectedEntries
		ifAbsentPut: [ Set new ]
]

{ #category : #accessing }
DRDispatchTableApp >> selectedEntries: aCollection [
	"Answer a <Collection> with the entries marked for installation"

	^ self
		propertyAt: #selectedEntries
		put: aCollection asSet
]

{ #category : #running }
DRDispatchTableApp >> start [
	" Private - See superimplementor's comment "

	(DRDispatchTableBuilderUI newApplication: self) open
]
