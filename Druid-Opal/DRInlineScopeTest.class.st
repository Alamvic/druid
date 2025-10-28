Class {
	#name : 'DRInlineScopeTest',
	#superclass : 'DROptimisationTest',
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal',
	#tag : 'Tests'
}

{ #category : 'transforming' }
DRInlineScopeTest >> inline: aDRMessageSend [ 

	DRInline new inline: aDRMessageSend 
]

{ #category : 'as yet unclassified' }
DRInlineScopeTest >> interpreterClass [

	^ DrOpalExamples 
]

{ #category : 'running' }
DRInlineScopeTest >> newCompiler [

	^ DRMethodCompilerCompiler new
]

{ #category : 'tests' }
DRInlineScopeTest >> testInlineMethodCollisionTemporaries [

	| cfg inlinedScope innerInlinedScope inlinedStores |
	compilerCompiler irGenerator typeSystem:
		DRCustomisationTypeSystem new.
	compilerCompiler interpreter: self interpreterClass new.

	cfg := self generateDruidIRFor: #methodBlockNested2ReadAndWrite.
	self inline: cfg messageSends first.
	self inline: cfg messageSends first.

	self assert: cfg messageSends isEmpty.

	"Scopes: InnerInlined -> Inlined -> Method"
	inlinedScope := cfg scope inlinedScopes unique.
	innerInlinedScope := inlinedScope inlinedScopes unique.
	self assert: innerInlinedScope outerScope equals: inlinedScope.
	self assert: inlinedScope outerScope equals: cfg scope.

	"All temps where inlined in different vectors"
	self assert: cfg scope argumentNames isEmpty.
	self assert: cfg scope tempVarNames equals: #( temp1 temp2 ).
	self assert: cfg scope definedTempVectors equals: {
			('0vector0' -> #( #temp1 #temp2 )).
			('0vector1' -> #( #tmp1 #aBlock #x )).
			('0vector2' -> #( #tmp1 #aBlock #y )) } asOrderedDictionary.
	self assert: cfg scope copiedVars isEmpty.

	"Each instruction point to the correct vector"
	inlinedStores := cfg instructions select: [ :i |
		                 i isStore and: [ i address value = #tmp1 ] ].
	inlinedStores sort: [ :i1 :i2 | i2 isDominatedBy: i1 ].
	self assert: inlinedStores size equals: 2.
	self assert: inlinedStores first scope equals: innerInlinedScope.
	self assert: inlinedStores second scope equals: inlinedScope
]

{ #category : 'tests' }
DRInlineScopeTest >> testInlineOneMethodBlock [

	| cfg blockCFG inlinedScope |
	compilerCompiler irGenerator typeSystem:
		DRCustomisationTypeSystem new.
	compilerCompiler interpreter: self interpreterClass new.

	cfg := self generateDruidIRFor: #methodBlockNested2ReadAndWrite.
	self inline: cfg messageSends first.
	blockCFG := cfg messageSends first arguments first ensureIR.

	"Scopes: Block -> Inlined -> Method"
	inlinedScope := blockCFG scope outerScope.
	self assert: inlinedScope outerScope equals: cfg scope.
	self
		assertCollection: cfg scope inlinedScopes
		hasSameElements: { inlinedScope }.

	self assert: cfg scope argumentNames isEmpty.
	self assert: cfg scope tempVarNames equals: #( temp1 temp2 ).
	self assert: cfg scope definedTempVectors equals: {
			('0vector0' -> #( #temp1 #temp2 )).
			('0vector1' -> #( #tmp1 #aBlock #x )) } asOrderedDictionary.
	self assert: cfg scope copiedVars isEmpty.

	self assert: blockCFG scope argumentNames isEmpty.
	self assert: blockCFG scope tempVarNames equals: #( y ).
	self assert: blockCFG scope tempVectorVarNames equals: #( y ).
	self assert: blockCFG scope copiedVars equals: {
			('0vector0' -> #( #temp1 #temp2 )).
			('0vector1' -> #( #tmp1 #aBlock #x )) } asOrderedDictionary 
]
