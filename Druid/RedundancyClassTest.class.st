Class {
	#name : #RedundancyClassTest,
	#superclass : #DRSSAPRETest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
RedundancyClassTest >> test2DiffAddExpressionHasDiffClassNumber [


	| cfg instruction1 instruction2 |
	cfg := self setUpCFG: 3.
	instruction1 := cfg b1 add: 1 to: 3.
	instruction2 := cfg b2 add: 1 to: 5.

	self assert: (valueNumberer valueNumberOf: instruction1) equals: 1.
	self assert: (valueNumberer valueNumberOf: instruction2) equals: 2.
]

{ #category : #tests }
RedundancyClassTest >> test2DiffConstexpressHasDiffClassNumber [

	| cfg instruction1 instruction2 |
	cfg := self setUpCFG: 2.
	instruction1 := cfg b1 copy: 1.
	instruction2 := cfg b2 copy: 3.

	self assert: (valueNumberer valueNumberOf: instruction1) equals: 1.
	self assert: (valueNumberer valueNumberOf: instruction2) equals: 2.
]

{ #category : #tests }
RedundancyClassTest >> test2SameAddExpressionHasSameClassNumber [
	
	| cfg instruction1 instruction2 |
	
	cfg := self setUpCFG: 3.
	instruction1 := cfg b1 add: 1 to: 5.
	instruction2 := cfg b2 add: 1 to: 5.
	
	self
		assert: (valueNumberer valueNumberOf: instruction1)
		equals: (valueNumberer valueNumberOf: instruction2)
]

{ #category : #tests }
RedundancyClassTest >> test2SameAddExpressionHasSameClassNumberCommutative [
	
	| cfg instruction1 instruction2 |
	
	cfg := self setUpCFG: 3.
	instruction1 := cfg b1 add: 1 to: 5.
	instruction2 := cfg b2 add: 5 to: 1.
	
	self
		assert: (valueNumberer valueNumberOf: instruction1)
		equals: (valueNumberer valueNumberOf: instruction2)
]

{ #category : #tests }
RedundancyClassTest >> test2SameConstexpressHasSameClassNumber [

	| cfg instruction1 instruction2 |
	cfg := self setUpCFG: 2.
	instruction1 := cfg b1 copy: 3.
	instruction2 := cfg b2 copy: 3.

	self
		assert: (valueNumberer valueNumberOf: instruction1)
		equals: (valueNumberer valueNumberOf: instruction2)
]

{ #category : #tests }
RedundancyClassTest >> test3DiffConstexpressHasCorrectClassNumber [

	| cfg instruction1 instruction2 instruction3 |
	cfg := self setUpCFG: 3.
	instruction1 := cfg b1 copy: 1.
	instruction2 := cfg b2 copy: 3.
	instruction3 := cfg b3 copy: 5.


	self assert: (valueNumberer valueNumberOf: instruction1) equals: 1.
	self assert: (valueNumberer valueNumberOf: instruction2) equals: 2.
	self assert: (valueNumberer valueNumberOf: instruction3) equals: 3.
	
]

{ #category : #tests }
RedundancyClassTest >> test3SameConstexpressHasCorrectClassNumber [

	| cfg instruction1 instruction2 instruction3 |
	cfg := self setUpCFG: 3.
	instruction1 := cfg b1 copy: 3.
	instruction2 := cfg b2 copy: 3.
	instruction3 := cfg b3 copy: 3.


	self assert: (valueNumberer valueNumberOf: instruction1) equals: 1.
	self assert: (valueNumberer valueNumberOf: instruction2) equals: 1.
	self assert: (valueNumberer valueNumberOf: instruction3) equals: 1.
	
]

{ #category : #tests }
RedundancyClassTest >> testConstantFoldingExpressionHasSameClassNumber [
	"C1 = 1
	 1 + C1 is in the same class of 1 + 1
	"
	
| cfg |
cfg := self setUpCFG:1.
]
