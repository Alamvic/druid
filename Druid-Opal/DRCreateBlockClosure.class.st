Class {
	#name : 'DRCreateBlockClosure',
	#superclass : 'DRInstruction',
	#instVars : [
		'blockCFG',
		'blockNode',
		'outerContext'
	],
	#category : 'Druid-Opal-IR',
	#package : 'Druid-Opal',
	#tag : 'IR'
}

{ #category : 'accessing' }
DRCreateBlockClosure >> blockCFG [
	^ blockCFG
]

{ #category : 'as yet unclassified' }
DRCreateBlockClosure >> blockCFG: aDRControlFlowGraph [

	blockCFG := aDRControlFlowGraph
]

{ #category : 'accessing' }
DRCreateBlockClosure >> blockNode: anOCBlockNode [

	blockNode := anOCBlockNode
]

{ #category : 'accessing' }
DRCreateBlockClosure >> codeNode [

	^ blockNode
]

{ #category : 'inspecting' }
DRCreateBlockClosure >> inspectionCFG: aBuilder [

	<inspectorPresentationOrder: 10 title: 'CFG'>
	| view |
	view := RSCanvas new.
	^ blockCFG asInspectorIn: view
]

{ #category : 'testing' }
DRCreateBlockClosure >> isBlock [

	^ true
]

{ #category : 'testing' }
DRCreateBlockClosure >> isCreateBlockClosure [

	^ true
]

{ #category : 'accessing' }
DRCreateBlockClosure >> outerContext [
	^ outerContext
]

{ #category : 'accessing' }
DRCreateBlockClosure >> outerContext: aDRClosureStackFrame [

	outerContext := aDRClosureStackFrame
]

{ #category : 'types' }
DRCreateBlockClosure >> typeWithAlreadySeen: aCollection [

	^ BlockClosure asDRType
]
