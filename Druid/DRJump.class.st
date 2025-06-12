Class {
	#name : 'DRJump',
	#superclass : 'DRInstruction',
	#instVars : [
		'target',
		'isBackJump'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'testing' }
DRJump class >> getDependenciesFromOperandsString: operands withOpcode: opcode [

	^ { nil. { operands } }
]

{ #category : 'visiting' }
DRJump >> = aJump [

	super = aJump ifFalse: [ ^ false ].
	^ target = aJump target
]

{ #category : 'visiting' }
DRJump >> acceptVisitor: aVisitor [
	
	^ aVisitor visitJump: self
]

{ #category : 'converting' }
DRJump >> asBackJumpLoop [

	^ self controlFlowGraph allLoops
		  detect: [ :loop | loop backJump = self ]
		  ifNone: nil
]

{ #category : 'accessing' }
DRJump >> backJumpTargets [

	^ isBackJump
		  ifTrue: [ Set with: target ]
		  ifFalse: [ Set new ]
]

{ #category : 'accessing' }
DRJump >> beBackJump [
	
	isBackJump := true
]

{ #category : 'accessing' }
DRJump >> beBackJumpTo: aDRBasicBlock [

	self assert: aDRBasicBlock = target.
	self beBackJump
]

{ #category : 'accessing' }
DRJump >> beForwardJump [
	
	isBackJump := false
]

{ #category : 'accessing' }
DRJump >> beForwardJumpTo: aDRBasicBlock [

	self assert: aDRBasicBlock = target.
	self beForwardJump
]

{ #category : 'testing' }
DRJump >> canMoveOutsideLoop: backJump [

	^ false
]

{ #category : 'visiting' }
DRJump >> hash [

	^ super hash + target hash
]

{ #category : 'initialization' }
DRJump >> initialize [

	super initialize.
	result := DRNoRegister new.
	isBackJump := false
]

{ #category : 'inspecting' }
DRJump >> inspectionSource: aBuilder [
	<inspectorPresentationOrder: 0 title: 'Source'>

	^ SpCodePresenter new
		text: target asString;
		yourself
]

{ #category : 'testing' }
DRJump >> isBackJump [

	^ isBackJump
]

{ #category : 'testing' }
DRJump >> isBackJumpLoop [

	^ self asBackJumpLoop isNotNil

]

{ #category : 'testing' }
DRJump >> isBackJumpTo: aDRBasicBlock [

	^ isBackJump and: [ target = aDRBasicBlock ]
]

{ #category : 'accessing' }
DRJump >> isJump [ 

	^ true
]

{ #category : 'accessing' }
DRJump >> isMandatoryInstruction [
	
	^ true
]

{ #category : 'accessing' }
DRJump >> isNonConditionalJump [
	
	^ true
]

{ #category : 'accessing' }
DRJump >> loop [
	^ self isBackJump ifTrue: [ DRLoop fromBackJump: self ] ifFalse: [ self error: 'Cannot obtain loop from forward jump'].
]

{ #category : 'printing' }
DRJump >> opcode [
	
	^ 'Jump'
]

{ #category : 'printing' }
DRJump >> prettyPrint [

	^ 'Jump -> ', target id asString
]

{ #category : 'printing' }
DRJump >> printIROperands [

	^ ('b' , self target id asString)
]

{ #category : 'replacing' }
DRJump >> replaceTarget: previousTarget by: newTarget [
	
	target = previousTarget ifFalse: [ self error: 'Invalid target' ].
	target := newTarget.
]

{ #category : 'accessing' }
DRJump >> result: r [

]

{ #category : 'SCCP' }
DRJump >> sccpEvaluateFor: sccp [

	sccp addJumpToCFGWorklist: self
]

{ #category : 'SCCP' }
DRJump >> sccpLatticeValueFor: sccp [

	^ sccp top
]

{ #category : 'accessing' }
DRJump >> target [
	^ target
]

{ #category : 'accessing' }
DRJump >> target: aBasicBlock [ 
	
	target := aBasicBlock
]

{ #category : 'accessing' }
DRJump >> targets [
	
	^ { target }
]

{ #category : 'accessing' }
DRJump >> targets: aCollection [ 
	self target: aCollection unique.
]

{ #category : 'accessing' }
DRJump >> validate [

	super validate.

	(self basicBlock controlFlowGraph blocks includes: target) ifFalse: [ 
			DRError signal: 'Invalid jump target ', target id asString, ' in: ' , self prettyPrint ].

]
