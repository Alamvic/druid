Class {
	#name : #DRJump,
	#superclass : #DRInstruction,
	#instVars : [
		'target',
		'isBackJump'
	],
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRJump >> = aJump [

	super = aJump ifFalse: [ ^ false ].
	^ target = aJump target
]

{ #category : #visiting }
DRJump >> acceptVisitor: aVisitor [
	
	^ aVisitor visitJump: self
]

{ #category : #accessing }
DRJump >> backJumpTargets [

	^ isBackJump
		  ifTrue: [ Set with: target ]
		  ifFalse: [ Set new ]
]

{ #category : #accessing }
DRJump >> beBackJump [
	
	isBackJump := true
]

{ #category : #accessing }
DRJump >> beBackJumpTo: aDRBasicBlock [

	self assert: aDRBasicBlock = target.
	self beBackJump
]

{ #category : #accessing }
DRJump >> beForwardJump [
	
	isBackJump := false
]

{ #category : #accessing }
DRJump >> beForwardJumpTo: aDRBasicBlock [

	self assert: aDRBasicBlock = target.
	self beForwardJump
]

{ #category : #testing }
DRJump >> canMoveOutsideLoop: backJump [

	^ false
]

{ #category : #visiting }
DRJump >> hash [

	^ super hash + target hash
]

{ #category : #initialization }
DRJump >> initialize [

	super initialize.
	result := DRNoRegister new.
	isBackJump := false
]

{ #category : #inspecting }
DRJump >> inspectionSource: aBuilder [
	<inspectorPresentationOrder: 0 title: 'Source'>

	^ SpCodePresenter new
		text: target asString;
		yourself
]

{ #category : #testing }
DRJump >> isBackJump [

	^ isBackJump
]

{ #category : #testing }
DRJump >> isBackJumpTo: aDRBasicBlock [

	^ isBackJump and: [ target = aDRBasicBlock ]
]

{ #category : #accessing }
DRJump >> isJump [ 

	^ true
]

{ #category : #accessing }
DRJump >> isMandatoryInstruction [
	
	^ true
]

{ #category : #accessing }
DRJump >> isNonConditionalJump [
	
	^ true
]

{ #category : #accessing }
DRJump >> loop [
	^ self isBackJump ifTrue: [ DRLoop fromBackJump: self ] ifFalse: [ self error: 'Cannot obtain loop from forward jump'].
]

{ #category : #printing }
DRJump >> opcode [
	
	^ 'Jump'
]

{ #category : #printing }
DRJump >> prettyPrint [

	^ 'Jump -> ', target id asString
]

{ #category : #replacing }
DRJump >> replaceTarget: previousTarget by: newTarget [
	
	target = previousTarget ifFalse: [ self error: 'Invalid target' ].
	target := newTarget.
]

{ #category : #initialization }
DRJump >> result: r [

	self halt.
]

{ #category : #SCCP }
DRJump >> sccpEvaluateFor: sccp [

	sccp addJumpToCFGWorklist: self
]

{ #category : #SCCP }
DRJump >> sccpLatticeValueFor: sccp [

	^ sccp top
]

{ #category : #accessing }
DRJump >> target [
	^ target
]

{ #category : #accessing }
DRJump >> target: aBasicBlock [ 
	
	target := aBasicBlock
]

{ #category : #accessing }
DRJump >> targets [
	
	^ { target }
]
