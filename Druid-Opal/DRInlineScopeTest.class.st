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

	| cfg blockCFG inlinedBlockScope inlinedMethodScope |
	compilerCompiler irGenerator typeSystem:
		DRCustomisationTypeSystem new.
	compilerCompiler interpreter: self interpreterClass new.

	cfg := self generateDruidIRFor: #methodBlockNested2ReadAndWrite.
	self inline: cfg messageSends first.
	cfg applyOptimisation: (DRCleanScopes then: DRDeadCodeElimination). "Clean a bit for quering"
	blockCFG := cfg allBlockClosures unique ensureIR.

	"Scopes: Block -> Inlined (block) -> Inlined (method) -> Method"
	inlinedBlockScope := blockCFG scope outerScope.
	inlinedMethodScope := inlinedBlockScope outerScope.
	self assert: inlinedMethodScope outerScope equals: cfg scope.
	
	self assert: cfg scope inlinedScopes unique equals: inlinedMethodScope.
	self assert: inlinedMethodScope inlinedScopes unique equals: inlinedBlockScope.
	
	self assert: cfg scope argumentNames isEmpty.
	self assert: cfg scope tempVarNames equals: #( temp1 temp2 ).
	self assert: cfg scope definedTempVectors equals: {
			('0vector0' -> #( #temp1 #temp2 )) } asOrderedDictionary.
	self assert: cfg scope copiedVars isEmpty.

	self assert: inlinedMethodScope argumentNames isEmpty.
	self assert: inlinedMethodScope tempVarNames equals: #( #tmp1 #aBlock ).
	self assert: inlinedMethodScope definedTempVectors equals: {
			('0vector1' -> #( #tmp1 #aBlock )) } asOrderedDictionary.
	self assert: inlinedMethodScope copiedVars equals: {
			('0vector0' -> #( #temp1 #temp2 )) } asOrderedDictionary.

	self assert: inlinedBlockScope argumentNames isEmpty.
	self assert: inlinedBlockScope tempVarNames equals: #( x ).
	self assert: inlinedBlockScope definedTempVectors equals: {
			('0vector2' -> #( x )) } asOrderedDictionary.
	self assert: inlinedBlockScope copiedVars equals: {
			('0vector0' -> #( #temp1 #temp2 )).
			('0vector1' -> #( #tmp1 #aBlock )) } asOrderedDictionary.

	self assert: blockCFG scope argumentNames isEmpty.
	self assert: blockCFG scope tempVarNames equals: #( y ).
	self assert: blockCFG scope definedTempVectors equals: {
			('0vector3' -> #( y )) } asOrderedDictionary.
	self assert: blockCFG scope copiedVars equals: {
			('0vector0' -> #( #temp1 #temp2 )).
			('0vector1' -> #( #tmp1 #aBlock )).
			('0vector2' -> #( x )) } asOrderedDictionary 
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

	| cfg inlinedMethodScope1 inlinedBlockScope1 inlinedStores inlinedMethodScope2 inlinedBlockScope2 |
	compilerCompiler irGenerator typeSystem:
		DRCustomisationTypeSystem new.
	compilerCompiler interpreter: self interpreterClass new.

	cfg := self generateDruidIRFor: #methodBlockNested2ReadAndWrite.
	self inline: cfg messageSends first.
	self inline: cfg messageSends first.

	self assert: cfg messageSends isEmpty.

	"Scopes: Inlined (block) -> Inlined (method) -> Inlined (block) -> Inlined (method) -> Method"
	inlinedMethodScope1 := cfg scope inlinedScopes unique.
	inlinedBlockScope1 := inlinedMethodScope1 inlinedScopes unique.
	inlinedMethodScope2 := inlinedBlockScope1 inlinedScopes unique.
	inlinedBlockScope2 := inlinedMethodScope2 inlinedScopes unique.
	
	self assert: inlinedMethodScope1 outerScope equals: cfg scope.
	self assert: inlinedBlockScope1 outerScope equals: inlinedMethodScope1.
	self assert: inlinedMethodScope2 outerScope equals: inlinedBlockScope1.
	self assert: inlinedBlockScope2 outerScope equals: inlinedMethodScope2.

	"All temps are inlined in different vectors"
	self assert: cfg scope argumentNames isEmpty.
	self assert: cfg scope tempVarNames equals: #( temp1 temp2 ).
	self assert: cfg scope definedTempVectors equals: {
			('0vector0' -> #( #temp1 #temp2 )) } asOrderedDictionary.
	self assert: cfg scope copiedVars isEmpty.

	self assert: inlinedMethodScope1 argumentNames isEmpty.
	self assert: inlinedMethodScope1 tempVarNames equals: #( #tmp1 #aBlock ).
	self assert: inlinedMethodScope1 definedTempVectors equals: {
			('0vector1' -> #( #tmp1 #aBlock )) } asOrderedDictionary.
	self assert: inlinedMethodScope1 copiedVars equals: {
			('0vector0' -> #( #temp1 #temp2 )) } asOrderedDictionary.

	self assert: inlinedBlockScope1 argumentNames isEmpty.
	self assert: inlinedBlockScope1 tempVarNames equals: #( x ).
	self assert: inlinedBlockScope1 definedTempVectors equals: {
			('0vector2' -> #( x )) } asOrderedDictionary.
	self assert: inlinedBlockScope1 copiedVars equals: {
			('0vector0' -> #( #temp1 #temp2 )).
			('0vector1' -> #( #tmp1 #aBlock )) } asOrderedDictionary.

	self assert: inlinedMethodScope2 argumentNames isEmpty.
	self assert: inlinedMethodScope2 tempVarNames equals: #( #tmp1 #aBlock ).
	self assert: inlinedMethodScope2 definedTempVectors equals: {
			('0vector3' -> #( #tmp1 #aBlock )) } asOrderedDictionary.
	self assert: inlinedMethodScope2 copiedVars equals: {
			('0vector0' -> #( #temp1 #temp2 )).
			('0vector1' -> #( #tmp1 #aBlock )).
			('0vector2' -> #( x )) } asOrderedDictionary.

	self assert: inlinedBlockScope2 argumentNames isEmpty.
	self assert: inlinedBlockScope2 tempVarNames equals: #( y ).
	self assert: inlinedBlockScope2 definedTempVectors equals: {
			('0vector4' -> #( y )) } asOrderedDictionary.
	self assert: inlinedBlockScope2 copiedVars equals: {
			('0vector0' -> #( #temp1 #temp2 )).
			('0vector1' -> #( #tmp1 #aBlock )).
			('0vector2' -> #( x )).
			('0vector3' -> #( #tmp1 #aBlock )) } asOrderedDictionary.

	"Each instruction point to the correct vector"
	inlinedStores := cfg instructions select: [ :i |
		                 i isStore and: [ i variableName = #tmp1 ] ].
	inlinedStores sort: [ :i1 :i2 | i1 isDominatedBy: i2 ].
	self assert: inlinedStores size equals: 2.
	self assert: inlinedStores first scope equals: inlinedMethodScope1.
	self assert: inlinedStores second scope equals: inlinedMethodScope2
]
