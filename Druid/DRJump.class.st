Class {
	#name : #DRJump,
	#superclass : #DRInstruction,
	#instVars : [
		'target'
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

{ #category : #visiting }
DRJump >> hash [

	^ super hash + target hash
]

{ #category : #accessing }
DRJump >> isJump [ 

	^ true
]

{ #category : #printing }
DRJump >> prettyPrint [

	^ 'Jump -> ', target id asString
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
