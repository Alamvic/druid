Class {
	#name : 'DRInlineScopeTest',
	#superclass : 'DRMethodOptimizationTest',
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal',
	#tag : 'Tests'
}

{ #category : 'transforming' }
DRInlineScopeTest >> inline: aDRMessageSend [ 

	DRInline new inline: aDRMessageSend 
]

{ #category : 'tests' }
DRInlineScopeTest >> setUp [

	super setUp.
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	compilerCompiler interpreter: self inputClass new
]

{ #category : 'tests' }
DRInlineScopeTest >> testInlineMethodBlock [

	| cfg blockCFG inlinedBlockScope inlinedMethodScope send |
	cfg := self generateDruidIRFor: #methodBlockNested2ReadAndWrite.
	send := cfg messageSends first.
	self inline: send.
	cfg applyOptimisation: (DRCleanScopes then: DRDeadCodeElimination). "Clean a bit for quering"
	blockCFG := cfg allBlockClosures unique ensureIR.

	"Scopes: Block -> Inlined (block) -> Inlined (method) -> Send (empty) -> Method"
	inlinedBlockScope := blockCFG scope outerScope.
	inlinedMethodScope := inlinedBlockScope outerScope.
	self assert: inlinedMethodScope outerScope equals: send scope.
	self assert: send scope outerScope equals: cfg scope.
	
	self assert: cfg scope inlinedScopes unique equals: send scope.
	self assert: send scope inlinedScopes unique equals: inlinedMethodScope.
	self assert: inlinedMethodScope inlinedScopes unique equals: inlinedBlockScope.
	
	self assert: cfg scope argumentNames isEmpty.
	self assert: cfg scope tempVarNames equals: #( temp1 temp2 ).
	self assert: cfg scope definedTempVectors equals: {
			('0vector0' -> #( #temp1 #temp2 )) } asOrderedDictionary.
	self assert: cfg scope copiedVars isEmpty.
	
	self assert: send scope isEmpty.

	self assert: inlinedMethodScope argumentNames isEmpty.
	self assert: inlinedMethodScope tempVarNames equals: #( #tmp1 #aBlock ).
	self assert: inlinedMethodScope definedTempVectors equals: {
			('0vector2' -> #( #tmp1 #aBlock )) } asOrderedDictionary.
	self assert: inlinedMethodScope copiedVars equals: {
			('0vector0' -> #( #temp1 #temp2 )) } asOrderedDictionary.

	self assert: inlinedBlockScope argumentNames isEmpty.
	self assert: inlinedBlockScope tempVarNames equals: #( x ).
	self assert: inlinedBlockScope definedTempVectors equals: {
			('0vector3' -> #( x )) } asOrderedDictionary.
	self assert: inlinedBlockScope copiedVars equals: {
			('0vector0' -> #( #temp1 #temp2 )).
			('0vector2' -> #( #tmp1 #aBlock )) } asOrderedDictionary.

	self assert: blockCFG scope argumentNames isEmpty.
	self assert: blockCFG scope tempVarNames equals: #( y ).
	self assert: blockCFG scope definedTempVectors equals: {
			('0vector4' -> #( y )) } asOrderedDictionary.
	self assert: blockCFG scope copiedVars equals: {
			('0vector0' -> #( #temp1 #temp2 )).
			('0vector2' -> #( #tmp1 #aBlock )).
			('0vector3' -> #( x )) } asOrderedDictionary 
]

{ #category : 'tests' }
DRInlineScopeTest >> testInlineMethodBlockTwoOuterScopes [

	| cfg blockCFG inlinedScope valueMessage blockScope blockReturn send |
	cfg := self generateDruidIRFor: #methodBlockTwoOuterScopes.
	send := cfg messageSends first.
	self inline: send.
	blockCFG := 	cfg allBlockClosures unique ensureIR.

	"Scopes 1: Block -> Inlined -> Send (empty) -> Method"
	blockScope := blockCFG scope.
	inlinedScope := blockScope outerScope. 
	self assert: inlinedScope outerScope equals: send scope.
	self assert: send scope outerScope equals: cfg scope.
	self assert: cfg scope inlinedScopes first equals: send scope.

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

	self assert: send scope isEmpty.

	self assert: inlinedScope argumentNames isEmpty.
	self assert: inlinedScope tempVarNames equals: #( #arg ).
	self assert: inlinedScope definedTempVectors equals: {
			('0vector2' -> #( #arg )) } asOrderedDictionary.
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

	| cfg inlinedMethodScope1 inlinedBlockScope1 inlinedStores inlinedMethodScope2 inlinedBlockScope2 send1 send2 sendScope1 sendScope2 |
	cfg := self generateDruidIRFor: #methodBlockNested2ReadAndWrite.
	send1 := cfg messageSends first.
	self inline: send1.
	send2 := cfg messageSends first.
	self inline: send2.

	self assert: cfg messageSends isEmpty.

	"Scopes: Inlined (block) -> Inlined (method) -> Send (empty) -> Inlined (block) -> Inlined (method) -> Send (empty) -> Method"
	sendScope1 := cfg scope inlinedScopes unique.
	self assert: sendScope1 equals: send1 scope.
	inlinedMethodScope1 := sendScope1 inlinedScopes unique.
	self assert: inlinedMethodScope1 node isMethod.
	inlinedBlockScope1 := inlinedMethodScope1 inlinedScopes unique.
	self assert: inlinedBlockScope1 node isBlock.
	sendScope2 := inlinedBlockScope1 inlinedScopes unique.
	self assert: sendScope2 equals: send2 scope.
	inlinedMethodScope2 := sendScope2 inlinedScopes unique.
	self assert: inlinedMethodScope2 node isMethod.
	inlinedBlockScope2 := inlinedMethodScope2 inlinedScopes unique.
	self assert: inlinedBlockScope2 node isBlock.
	
	self assert: sendScope1 outerScope equals: cfg scope.
	self assert: inlinedMethodScope1 outerScope equals: sendScope1.
	self assert: inlinedBlockScope1 outerScope equals: inlinedMethodScope1.
	self assert: sendScope2 outerScope equals: inlinedBlockScope1.
	self assert: inlinedMethodScope2 outerScope equals: sendScope2.
	self assert: inlinedBlockScope2 outerScope equals: inlinedMethodScope2.

	"All temps are inlined in different vectors"
	self assert: cfg scope argumentNames isEmpty.
	self assert: cfg scope tempVarNames equals: #( temp1 temp2 ).
	self assert: cfg scope definedTempVectors equals: {
			('0vector0' -> #( #temp1 #temp2 )) } asOrderedDictionary.
	self assert: cfg scope copiedVars isEmpty.

	self assert: send1 scope isEmpty.

	self assert: inlinedMethodScope1 argumentNames isEmpty.
	self assert: inlinedMethodScope1 tempVarNames equals: #( #tmp1 #aBlock ).
	self assert: inlinedMethodScope1 definedTempVectors equals: {
			('0vector2' -> #( #tmp1 #aBlock )) } asOrderedDictionary.
	self assert: inlinedMethodScope1 copiedVars equals: {
			('0vector0' -> #( #temp1 #temp2 )) } asOrderedDictionary.

	self assert: inlinedBlockScope1 argumentNames isEmpty.
	self assert: inlinedBlockScope1 tempVarNames equals: #( x ).
	self assert: inlinedBlockScope1 definedTempVectors equals: {
			('0vector3' -> #( x )) } asOrderedDictionary.
	self assert: inlinedBlockScope1 copiedVars equals: {
			('0vector0' -> #( #temp1 #temp2 )).
			('0vector2' -> #( #tmp1 #aBlock )) } asOrderedDictionary.

	self assert: send2 scope isEmpty.

	self assert: inlinedMethodScope2 argumentNames isEmpty.
	self assert: inlinedMethodScope2 tempVarNames equals: #( #tmp1 #aBlock ).
	self assert: inlinedMethodScope2 definedTempVectors equals: {
			('0vector5' -> #( #tmp1 #aBlock )) } asOrderedDictionary.
	self assert: inlinedMethodScope2 copiedVars equals: {
			('0vector0' -> #( #temp1 #temp2 )).
			('0vector2' -> #( #tmp1 #aBlock )).
			('0vector3' -> #( x )) } asOrderedDictionary.

	self assert: inlinedBlockScope2 argumentNames isEmpty.
	self assert: inlinedBlockScope2 tempVarNames equals: #( y ).
	self assert: inlinedBlockScope2 definedTempVectors equals: {
			('0vector6' -> #( y )) } asOrderedDictionary.
	self assert: inlinedBlockScope2 copiedVars equals: {
			('0vector0' -> #( #temp1 #temp2 )).
			('0vector2' -> #( #tmp1 #aBlock )).
			('0vector3' -> #( x )).
			('0vector5' -> #( #tmp1 #aBlock )) } asOrderedDictionary.

	"Each instruction point to the correct vector"
	inlinedStores := cfg instructions select: [ :i |
		                 i isStore and: [ i variableName = #tmp1 ] ].
	inlinedStores sort: [ :i1 :i2 | i1 isDominatedBy: i2 ].
	self assert: inlinedStores size equals: 2.
	self assert: inlinedStores first scope equals: inlinedMethodScope1.
	self assert: inlinedStores second scope equals: inlinedMethodScope2
]

{ #category : 'tests' }
DRInlineScopeTest >> testInlineMultiMethods [

	| cfg block1CFG block2CFG inlinedMethod1Scope send inlinedMethod2Scope |
	compilerCompiler irGenerator typeSystem: DRPragmaBasedTypeSystem new.
	cfg := self generateDruidIRFor: #methodWithMultipleTypeAnnotations:.
	send := cfg messageSends first.
	self inline: send.
	cfg applyOptimisation: (DRCleanScopes then: DRDeadCodeElimination then: DRPhiSimplication). "Clean a bit for quering"
	block1CFG := cfg allBlockClosures second ensureIR.
	block2CFG := cfg allBlockClosures last ensureIR.

	"Scopes1: Block1 -> Inlined1 (method) -> Send (empty) -> Method"
	inlinedMethod1Scope := block1CFG scope outerScope.
	self assert: inlinedMethod1Scope node isMethod.
	self assert: inlinedMethod1Scope outerScope equals: send scope.
	self assert: send scope inlinedScopes first equals: inlinedMethod1Scope.

	"Scopes2: Block2 -> Inlined2 (method) -> Send (empty) -> Method"
	inlinedMethod2Scope := block2CFG scope outerScope.
	self assert: inlinedMethod2Scope node isMethod.
	self assert: inlinedMethod2Scope outerScope equals: send scope.
	self assert: send scope inlinedScopes second equals: inlinedMethod2Scope.

	self assert: send scope outerScope equals: cfg scope.
	
	"Variables"
	self assert: cfg scope argumentNames equals: #(aCollection).
	self assert: cfg scope tempVarNames isEmpty.
	self assert: cfg scope definedTempVectors equals: {
			('0vector0' -> #( #aCollection )) } asOrderedDictionary.
	self assert: cfg scope copiedVars isEmpty.

	self assert: send scope isEmpty.

	"Inline 1"
	self assert: inlinedMethod1Scope argumentNames isEmpty.
	self assert: inlinedMethod1Scope tempVarNames equals: #( #newCollection #element #selectBlock ).
	self assert: inlinedMethod1Scope definedTempVectors equals: {
			('0vector2' -> #( #newCollection #element #selectBlock )) } asOrderedDictionary.
	self assert: inlinedMethod1Scope copiedVars equals: {
			('0vector0' -> #( #aCollection )) } asOrderedDictionary.

	self assert: block1CFG scope argumentNames equals: #( index ).
	self assert: block1CFG scope tempVarNames isEmpty.
	self assert: block1CFG scope definedTempVectors equals: {
			('0vector3' -> #( #index )) } asOrderedDictionary.
	self assert: block1CFG scope copiedVars equals: {
			('0vector0' -> #( #aCollection )).
			('0vector2' -> #( #newCollection #element #selectBlock )) } asOrderedDictionary.

	"Inline 2"
	self assert: inlinedMethod2Scope argumentNames isEmpty.
	self assert: inlinedMethod2Scope tempVarNames equals: #( #newCollection #aBlock ).
	self assert: inlinedMethod2Scope definedTempVectors equals: {
			('0vector2' -> #( #newCollection #aBlock )) } asOrderedDictionary.
	self assert: inlinedMethod2Scope copiedVars equals: {
			('0vector0' -> #( #aCollection )) } asOrderedDictionary.

	self assert: block2CFG scope argumentNames equals: #( #each ).
	self assert: block2CFG scope tempVarNames isEmpty.
	self assert: block2CFG scope definedTempVectors equals: {
			('0vector3' -> #( #each )) } asOrderedDictionary.
	self assert: block2CFG scope copiedVars equals: {
			('0vector0' -> #( #aCollection )).
			('0vector2' -> #( #newCollection #aBlock )) } asOrderedDictionary.


]
