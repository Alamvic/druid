Class {
	#name : 'DRScopeTest',
	#superclass : 'DRIRTest',
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal',
	#tag : 'Tests'
}

{ #category : 'test' }
DRScopeTest >> interpreterClass [

	^ DrOpalExamples 
]

{ #category : 'running' }
DRScopeTest >> newCompiler [

	^ DRMethodCompilerCompiler new
]

{ #category : 'tests' }
DRScopeTest >> testMethodNoArgsNoLocals [

	| cfg |
	cfg := self generateDruidIRFor: #empty.
	self assert: cfg scope isEmpty

]

{ #category : 'tests' }
DRScopeTest >> testMethodNoArgsTwoLocals [

	| cfg |
	cfg := self generateDruidIRFor: #simpleMethod.

	self assert: cfg scope argumentNames isEmpty.
	self assert: cfg scope tempVarNames equals: #( x y ).
	self assert: cfg scope tempVectorVarNames equals: #( x y )
]

{ #category : 'tests' }
DRScopeTest >> testMethodOneArgsNoLocals [

	| cfg |
	cfg := self generateDruidIRFor: #simpleMethodReturnArg:.
	
	self assert: cfg scope argumentNames equals: #( arg ).
	self assert: cfg scope tempVarNames isEmpty.
	self assert: cfg scope tempVectorVarNames equals: #( arg )
]

{ #category : 'tests' }
DRScopeTest >> testMethodOneArgsWithBlockOneLocals [

	| cfg blockCFG |
	cfg := self generateDruidIRFor: #simpleMethodWithBlockReadingArgVariable:.
	blockCFG := cfg messageSends first arguments first ensureIR.

	self assert: blockCFG scope outerScope equals: cfg scope.
	
	self assert: cfg scope argumentNames equals: #( arg ).
	self assert: cfg scope tempVarNames isEmpty.
	self assert: cfg scope tempVectorVarNames equals: #( arg ).
		
	self assert: blockCFG scope argumentNames isEmpty.
	self assert: blockCFG scope tempVarNames equals: #( x ).
	self assert: blockCFG scope tempVectorVarNames equals: #( x ).
	self assert: blockCFG scope copiedVars equals: {'0vector0' ->#( arg )} asOrderedDictionary
]

{ #category : 'tests' }
DRScopeTest >> testMethodTwoArgsOneLocals [

	| cfg |
	cfg := self generateDruidIRFor: #simpleMethodWith1Arg:arg2:.
	
	self assert: cfg scope argumentNames equals: #( argument1 argument2 ).
	self assert: cfg scope tempVarNames equals: #( x ).
	self assert: cfg scope tempVectorVarNames equals: #( argument1 argument2 x )
]

{ #category : 'tests' }
DRScopeTest >> testMethodWithBlockNoArgsOneLocals [

	| cfg blockCFG |
	cfg := self generateDruidIRFor: #simpleMethodWithBlock.
	blockCFG := cfg messageSends first arguments first ensureIR.
	
	self assert: blockCFG scope outerScope equals: cfg scope.
	
	self assert: cfg scope argumentNames isEmpty.
	self assert: cfg scope tempVarNames equals: #( tmp ).
	self assert: cfg scope tempVectorVarNames equals: #( tmp ).
	
	self assert: blockCFG scope argumentNames isEmpty.
	self assert: blockCFG scope tempVarNames equals: #( x ).
	self assert: blockCFG scope tempVectorVarNames equals: #( x ).
	self assert: blockCFG scope copiedVars equals: {'0vector0' ->#( tmp )} asOrderedDictionary
]

{ #category : 'tests' }
DRScopeTest >> testMethodWithBlockOneArgsOneLocals [

	| cfg blockCFG |
	cfg := self generateDruidIRFor: #simpleMethodWithBlockWithArg.
	blockCFG := cfg messageSends first arguments first ensureIR.
	
	self assert: blockCFG scope outerScope equals: cfg scope.
	
	self assert: cfg scope isEmpty.
	
	self assert: blockCFG scope argumentNames equals: #( arg ).
	self assert: blockCFG scope tempVarNames equals: #( x ).
	self assert: blockCFG scope tempVectorVarNames equals: #( arg x ).
	self assert: blockCFG scope copiedVars isEmpty
]

{ #category : 'tests' }
DRScopeTest >> testMethodWithNestedBlocksOneLocals [

	| cfg blockCFG innerBlockCFG |
	cfg := self generateDruidIRFor: #methodBlockNested2ReadingOuterTemp.
	blockCFG := cfg messageSends first arguments first ensureIR.
	innerBlockCFG := blockCFG messageSends first arguments first ensureIR.
	
	self assert: blockCFG scope outerScope equals: cfg scope.
	self assert: innerBlockCFG scope outerScope equals: blockCFG scope.
	
	self assert: cfg scope argumentNames isEmpty.
	self assert: cfg scope tempVarNames equals: #( temp ).
	self assert: cfg scope tempVectorVarNames equals: #( temp ).
	
	self assert: blockCFG scope argumentNames isEmpty.
	self assert: blockCFG scope tempVarNames equals: #( x ).
	self assert: blockCFG scope tempVectorVarNames equals: #( x ).
	self assert: blockCFG scope copiedVars equals: {'0vector0' ->#( temp )} asOrderedDictionary.
	
	self assert: innerBlockCFG scope argumentNames isEmpty.
	self assert: innerBlockCFG scope tempVarNames equals: #( y ).
	self assert: innerBlockCFG scope tempVectorVarNames equals: #( y ).
	self assert: innerBlockCFG scope copiedVars equals: {'0vector0' ->#( temp ) . '0vector1' ->#( x )} asOrderedDictionary
]
