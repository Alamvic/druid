Class {
	#name : #DRStackEffect,
	#superclass : #DRStackInstruction,
	#category : #'Druid-BytecodeToJITCompilation'
}

{ #category : #testing }
DRStackEffect >> isMandatoryInstruction [

	^ true
]

{ #category : #testing }
DRStackEffect >> isStackEffect [

	^ true
]

{ #category : #removing }
DRStackEffect >> removeFromCFG [
	"I'm a pop. If I'm removed, all my dependents should shift up by one their stack accesses"

	stackDependents do: [ :e |
		e shiftStackAccessBy: self stackDelta.
		e removeStackDependency: self ].

	super removeFromCFG.
	
	self controlFlowGraph rebuildStackDependencies.

]
