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
DRJump >> beBackJump [
	
	isBackJump := true
]

{ #category : #visiting }
DRJump >> hash [

	^ super hash + target hash
]

{ #category : #initialization }
DRJump >> initialize [

	super initialize.
	result := DRNoRegister new
]

{ #category : #testing }
DRJump >> isBackJump [
	^ isBackJump ifNil: [ false ]
]

{ #category : #accessing }
DRJump >> isJump [ 

	^ true
]

{ #category : #accessing }
DRJump >> isNonConditionalJump [
	
	^ true
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
