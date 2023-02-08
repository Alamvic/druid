Class {
	#name : #RedundancyClassTest,
	#superclass : #DRSSAPRETest,
	#category : #'Druid-Tests'
}

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
