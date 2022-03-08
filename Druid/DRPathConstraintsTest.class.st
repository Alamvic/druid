Class {
	#name : #DRPathConstraintsTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRPathConstraintsTest >> testCopyAddConstantConstraint [

	| cfg blocks |
	cfg := self generateDruidIRFor: #primitiveReturnOne.
	self optimize: cfg.
	self generatePaths: cfg.
	
	blocks := cfg reversePostOrderBlocks allButFirst.
	self assert: blocks first incomingRegInfo constraints isEmpty.
	self assert: blocks first outgoingRegInfo constraints size equals: 1.

]

{ #category : #tests }
DRPathConstraintsTest >> testFirstIncomingInfoHasNoConstraints [

	| cfg blocks |
	cfg := self generateDruidIRFor: #emptyPrimitive.
	self optimize: cfg.
	self generatePaths: cfg.
	
	blocks := cfg reversePostOrderBlocks allButFirst.
	self assert: blocks first incomingRegInfo constraints isEmpty.

]

{ #category : #tests }
DRPathConstraintsTest >> testJumpDoesNotAddConstraint [

	| cfg blocks |
	cfg := self generateDruidIRFor: #emptyPrimitive.
	self optimize: cfg.
	self generatePaths: cfg.
	
	blocks := cfg reversePostOrderBlocks allButFirst.
	self flag: #TODO. "Remove redundant copy"
	self assert: blocks first outgoingRegInfo constraints isEmpty.

]

{ #category : #tests }
DRPathConstraintsTest >> testLinearContigousBlocksShareConstraints [

	| cfg blocks |
	cfg := self generateDruidIRFor: #primitiveReturnOne.
	self optimize: cfg.
	self generatePaths: cfg.
	
	blocks := cfg reversePostOrderBlocks allButFirst.
	self assert: blocks first outgoingRegInfo equals: blocks second incomingRegInfo.

]

{ #category : #tests }
DRPathConstraintsTest >> testReturnDoesNotAddConstraint [

	| cfg returnedBlock |
	cfg := self generateDruidIRFor: #emptyPrimitive.
	self optimize: cfg.
	self generatePaths: cfg.
	
	returnedBlock := cfg reversePostOrderBlocks last.
	self assert: returnedBlock incomingRegInfo constraints equals: returnedBlock outgoingRegInfo constraints

]
