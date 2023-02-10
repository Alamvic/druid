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

	self
		deny: (valueNumberer valueNumberOf: instruction1)
		equals: (valueNumberer valueNumberOf: instruction2)
]

{ #category : #tests }
RedundancyClassTest >> test2DiffConstexpressHasDiffClassNumber [

	| cfg instruction1 instruction2 |
	cfg := self setUpCFG: 2.
	instruction1 := cfg b1 copy: 1.
	instruction2 := cfg b2 copy: 3.


	self
		deny: (valueNumberer valueNumberOf: instruction1)
		equals: (valueNumberer valueNumberOf: instruction2)
]

{ #category : #tests }
RedundancyClassTest >> test2DiffExpressionHasDiffClassNumberCommutative [
	"1 - 2  vs  2 - 1  != "
	| cfg instruction1 instruction2 |
	
	cfg := self setUpCFG: 2.
	instruction1 := cfg b1 sub: 1 from: 5.
	instruction2 := cfg b2 sub: 5 from: 1.

	self
		deny: (valueNumberer valueNumberOf: instruction1)
		equals: (valueNumberer valueNumberOf: instruction2)
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
	 1 + C1 is in the same class as 1 + 1
	"

	| cfg instruction1 instruction2 instruction3 |
	cfg := self setUpCFG: 2.
	instruction1 := cfg b1 copy: 1.
	instruction2 := cfg b1 add: 1 to: instruction1.
	instruction3 := cfg b2 add: 1 to: 1.

	self
		assert: (valueNumberer valueNumberOf: instruction2)
		equals: (valueNumberer valueNumberOf: instruction3)
]

{ #category : #tests }
RedundancyClassTest >> testConstantFoldingExpressionHasSameClassNumber2 [
	"T1 = 2
    T2 = T1
    T1 + T2  is in the same class as 2 + 2  =
	"

	| cfg instruction1 instruction2 instruction3 instruction4 |
	cfg := self setUpCFG: 2.

	instruction1 := cfg b1 copy: 2.
	instruction2 := cfg b1 copy: instruction1.
	instruction3 := cfg b1 add: instruction1 to: instruction2.
	instruction4 := cfg b2 add: 2 to: 2.
	self
		assert: (valueNumberer valueNumberOf: instruction3)
		equals: (valueNumberer valueNumberOf: instruction4)
]
