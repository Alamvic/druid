Class {
	#name : 'DRAbstractInstruction',
	#superclass : 'DRValue',
	#instVars : [
		'basicBlock'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'accessing' }
DRAbstractInstruction >> basicBlock [

	^ basicBlock
]

{ #category : 'accessing' }
DRAbstractInstruction >> basicBlock: anObject [

	basicBlock := anObject
]

{ #category : 'inspecting' }
DRAbstractInstruction >> inspectionDependencies: aBuilder [
	<inspectorPresentationOrder: 10 title: 'Dependency Graph'>
	
	| view blockViews eb |
	view := RSCanvas new.
	blockViews := self withAllDependencies asSet collect: [ :c | | g label |
		g := RSGroup new.
		(label := (RSLabel new text: c label asString)
			borderColor: Color black;
			yourself).
		g add: (RSBox new
			color: Color white;
			border: (RSBorder new color: Color purple; width: 2);
			extent: label extent + 10;
			yourself).
		g add: label.
		g asShape
			@ RSDraggable;
			model: c;
			yourself ] as: Array.
	view addAll: blockViews.
	
	eb := RSEdgeBuilder arrowedLine.
	eb canvas: view.
	eb withBorderAttachPoint.
	eb 	shapes: blockViews.
	eb connectToAll: #dependencies.
	
	"All the characters are displayed using a grid layout"
	RSDominanceTreeLayout new
		horizontalGap: 50;
		shouldValidateCycles;
		applyOn: blockViews.
	view @ RSCanvasController.
	
	^ SpRoassal3InspectorPresenter new
		canvas: view;
		yourself
]

{ #category : 'inspecting' }
DRAbstractInstruction >> inspectionSource: aBuilder [
	<inspectorPresentationOrder: 0 title: 'Source'>
	
	^ self originAST inspectionSourceCode
]

{ #category : 'inspecting' }
DRAbstractInstruction >> inspectionUsers: aBuilder [
	<inspectorPresentationOrder: 10 title: 'Users Graph'>
	
	| view blockViews eb |
	view := RSCanvas new.
	blockViews := self withAllUsers asSet collect: [ :c | | g label |
		g := RSGroup new.
		(label := (RSLabel new text: c label asString)
			borderColor: Color black;
			yourself).
		g add: (RSBox new
			color: Color white;
			border: (RSBorder new color: Color purple; width: 2);
			extent: label extent + 10;
			yourself).
		g add: label.
		g asShape
			@ RSDraggable;
			model: c;
			yourself ] as: Array.
	view addAll: blockViews.
	
	eb := RSEdgeBuilder arrowedLine.
	eb canvas: view.
	eb withBorderAttachPoint.
	eb 	shapes: blockViews.
	eb connectToAll: #users.
	
	"All the characters are displayed using a grid layout"
	RSDominanceTreeLayout new
		horizontalGap: 50;
		shouldValidateCycles;
		applyOn: blockViews.
	view @ RSCanvasController.
	
	^ SpRoassal3InspectorPresenter new
		canvas: view;
		yourself
]

{ #category : 'testing' }
DRAbstractInstruction >> isArithmeticInstruction [

	^ false
]

{ #category : 'testing' }
DRAbstractInstruction >> isBytecodeJump [
	
	^ false
]

{ #category : 'testing' }
DRAbstractInstruction >> isCall [
	
	^ false
]

{ #category : 'testing' }
DRAbstractInstruction >> isInterpreterReference [
	
	^ false
]

{ #category : 'testing' }
DRAbstractInstruction >> isLoadStackPointer [

	^ false
]

{ #category : 'testing' }
DRAbstractInstruction >> isLoadStackValue [
	
	^ false
]

{ #category : 'testing' }
DRAbstractInstruction >> isNegate [

	^ false
]

{ #category : 'testing' }
DRAbstractInstruction >> isPrimitiveFail [
	
	^ false
]

{ #category : 'testing' }
DRAbstractInstruction >> isScopedInstruction [

	^ false
]

{ #category : 'testing' }
DRAbstractInstruction >> isStackEffect [
	
	^ false
]

{ #category : 'accessing' }
DRAbstractInstruction >> nextInstruction [
	
	^ self basicBlock instructionAfter: self
]

{ #category : 'replacing' }
DRAbstractInstruction >> replaceBy: anotherInstruction [
	
	self = anotherInstruction ifTrue: [ ^ self ].

	basicBlock replace: self by: anotherInstruction.
	
	self replaceUsesBy: anotherInstruction.

	self operands asSet do: [ :instruction |
		instruction removeUser: self ].
]

{ #category : 'replacing' }
DRAbstractInstruction >> replaceByMaybeValue: aDRValue [

	aDRValue isInstruction ifTrue: [ ^ self replaceBy: aDRValue ].

	^ self replaceBy: (DRCopy operands: { aDRValue } result: self result)
]

{ #category : 'translating' }
DRAbstractInstruction >> rtlArgumentsForAbsoluteAddressForGenerator: generator [
	"We are translating ourselves to a Base+offset, where offset=0"

	"In cogit offset comes before base"
	^ {
		  (RBLiteralValueNode value: 0).
		  (RBVariableNode named: (generator allocateVariable: self result)) }
]

{ #category : 'translating' }
DRAbstractInstruction >> rtlOperandQualifierForAbsoluteAddress [
	
	^ #'M64:r:'
]

{ #category : 'inspecting' }
DRAbstractInstruction >> uiUnroll [

	| unrollingFactor |
	self isBackJump ifFalse: [ 
		^ self inform: 'Can only perform loop unrolling from back jumps' ].
	unrollingFactor := UIManager default
		                   request: 'Unrolling factor:'
		                   initialAnswer: '2'.
	(unrollingFactor ifNil: [ '' ]) ifEmpty: [ unrollingFactor := 2 ].
	self loop unroll: unrollingFactor asNumber
]

{ #category : 'validation' }
DRAbstractInstruction >> validate [

	self validateDependenciesAreUsedByMyself.
	self validateUsersUseMyself.

	self operands do: [ :o |
		(o isInstruction not or: [
			 (self isDominatedBy: o) or: [
				 "this is to handle the case we have different CFG, block is interpreted with a new CFG"
				 self controlFlowGraph ~= o controlFlowGraph ] ]) ifFalse: [
			DRError signal:
				'Invalid operand: ' , o asString , ' in: ' , self asString ] ]
]

{ #category : 'validation' }
DRAbstractInstruction >> validateDependenciesAreUsedByMyself [

	self dependencies do: [ :op | 
		(op hasUser: self) ifFalse: [ 
			DRError signal:
				'Dependency-user mismatch between: ' , self asString , ' and: '
				, op asString ] ]
]

{ #category : 'validation' }
DRAbstractInstruction >> validateUsersUseMyself [

	self users do: [ :user | 
		(user hasDependency: self) ifFalse: [ 
			DRError signal:
				'User-operand mismatch between: ' , user asString , ' and: '
				, self asString ] ]
]
