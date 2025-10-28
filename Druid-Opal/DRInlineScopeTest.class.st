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
DRInlineScopeTest >> testInlineMethodBlock [

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
	self assert: cfg scope inlinedScopes unique equals: inlinedScope.
	
	self assert: cfg scope argumentNames isEmpty.
	self assert: cfg scope tempVarNames equals: #( temp1 temp2 ).
	self assert: cfg scope definedTempVectors equals: {
			('0vector0' -> #( #temp1 #temp2 )) } asOrderedDictionary.
	self assert: cfg scope copiedVars isEmpty.

	self assert: inlinedScope argumentNames isEmpty.
	self assert: inlinedScope tempVarNames equals: #( #tmp1 #aBlock #x ).
	self assert: inlinedScope definedTempVectors equals: {
			('0vector1' -> #( #tmp1 #aBlock #x )) } asOrderedDictionary.
	self assert: inlinedScope copiedVars equals: {
			('0vector0' -> #( #temp1 #temp2 )) } asOrderedDictionary.

	self assert: blockCFG scope argumentNames isEmpty.
	self assert: blockCFG scope tempVarNames equals: #( y ).
	self assert: blockCFG scope tempVectorVarNames equals: #( y ).
	self assert: blockCFG scope copiedVars equals: {
			('0vector0' -> #( #temp1 #temp2 )).
			('0vector1' -> #( #tmp1 #aBlock #x )) } asOrderedDictionary 
]

{ #category : 'tests' }
DRInlineScopeTest >> testInlineMethodBlockTwoOuterScopes [

	| cfg blockCFG inlinedScope valueMessage blockScope blockReturn |
	compilerCompiler irGenerator typeSystem:
		DRCustomisationTypeSystem new.
	compilerCompiler interpreter: self interpreterClass new.

	cfg := self generateDruidIRFor: #methodBlockTwoOuterScopes.
	self inline: cfg messageSends first.
	blockCFG := 	cfg allBlockClosures unique ensureIR.

	"Scopes 1: Block -> Inlined -> Method"
	blockScope := blockCFG scope.
	inlinedScope := blockScope outerScope. 
	self assert: cfg scope inlinedScopes first equals: inlinedScope	.
	self assert: inlinedScope outerScope equals: cfg scope.

	"Scopes 2: Non-inlined (empty) -> Method"
	valueMessage := cfg messageSends unique.
	self
		assert: cfg scope inlinedScopes second
		equals: valueMessage inlineGenerator scope.
	self assert: valueMessage inlineGenerator scope isEmpty.

	"All temps are inlined in different vectors"
	self assert: cfg scope argumentNames isEmpty.
	self assert: cfg scope tempVarNames equals: #( temp block ).
	self assert: cfg scope definedTempVectors equals: {
			('0vector0' -> #( temp block )) } asOrderedDictionary.
	self assert: cfg scope copiedVars isEmpty.

	self assert: inlinedScope argumentNames isEmpty.
	self assert: inlinedScope tempVarNames equals: #( #arg ).
	self assert: inlinedScope definedTempVectors equals: {
			('0vector1' -> #( #arg )) } asOrderedDictionary.
	self assert: inlinedScope copiedVars equals: {
			('0vector0' -> #( temp block )) } asOrderedDictionary.

	"Each instruction point to the correct vector"
	blockReturn := blockCFG lastBasicBlock endInstruction.
	self assert: blockReturn isReturn.
	self assert: blockReturn returnedValue isLoadArgument.
	self assert: blockReturn returnedValue scope equals: inlinedScope
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

	"All temps are inlined in different vectors"
	self assert: cfg scope argumentNames isEmpty.
	self assert: cfg scope tempVarNames equals: #( temp1 temp2 ).
	self assert: cfg scope definedTempVectors equals: {
			('0vector0' -> #( #temp1 #temp2 )) } asOrderedDictionary.
	self assert: cfg scope copiedVars isEmpty.

	self assert: inlinedScope argumentNames isEmpty.
	self assert: inlinedScope tempVarNames equals: #( #tmp1 #aBlock #x ).
	self assert: inlinedScope definedTempVectors equals: {
			('0vector1' -> #( #tmp1 #aBlock #x )) } asOrderedDictionary.
	self assert: inlinedScope copiedVars equals: {
			('0vector0' -> #( #temp1 #temp2 )) } asOrderedDictionary.

	self assert: innerInlinedScope argumentNames isEmpty.
	self assert: innerInlinedScope tempVarNames equals: #( #tmp1 #aBlock #y ).
	self assert: innerInlinedScope definedTempVectors equals: {
			('0vector2' -> #( #tmp1 #aBlock #y )) } asOrderedDictionary.
	self assert: innerInlinedScope copiedVars equals: {
			('0vector0' -> #( #temp1 #temp2 )).
			('0vector1' -> #( #tmp1 #aBlock #x )) } asOrderedDictionary.

	"Each instruction point to the correct vector"
	inlinedStores := cfg instructions select: [ :i |
		                 i isStore and: [ i address value = #tmp1 ] ].
	inlinedStores sort: [ :i1 :i2 | i2 isDominatedBy: i1 ].
	self assert: inlinedStores size equals: 2.
	self assert: inlinedStores first scope equals: innerInlinedScope.
	self assert: inlinedStores second scope equals: inlinedScope
]
