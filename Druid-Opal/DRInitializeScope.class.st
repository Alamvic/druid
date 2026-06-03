Class {
	#name : 'DRInitializeScope',
	#superclass : 'DRInstruction',
	#instVars : [
		'scope'
	],
	#category : 'Druid-Opal-IR',
	#package : 'Druid-Opal',
	#tag : 'IR'
}

{ #category : 'accessing' }
DRInitializeScope class >> scope: aDRScope [

	| instruction |
	instruction := self result: DRNoRegister new.
	aDRScope initInstruction: instruction.
	^ instruction
		  scope: aDRScope;
		  yourself
]

{ #category : 'visiting' }
DRInitializeScope >> acceptVisitor: aVisitor [

	aVisitor visitInitializeScope: self
]

{ #category : 'inspecting' }
DRInitializeScope >> inspectionSource: aBuilder [
	<inspectorPresentationOrder: 0 title: 'Source'>
	
	^ scope node inspectionSource: aBuilder
]

{ #category : 'visiting' }
DRInitializeScope >> isMandatoryInstruction [

	^ true
]

{ #category : 'SCCP' }
DRInitializeScope >> sccpEvaluateFor: sccp [

	"Nothing to do"
]

{ #category : 'accessing' }
DRInitializeScope >> scope [

	^ scope
]

{ #category : 'accessing' }
DRInitializeScope >> scope: aDRScope [

	scope := aDRScope
]
