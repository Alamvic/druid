Class {
	#name : 'DRCleanScopesTests',
	#superclass : 'DROptimisationTest',
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal-Tests'
}

{ #category : 'running' }
DRCleanScopesTests >> setUp [

	super setUp.
	optimisation := DRCleanScopes new
]

{ #category : 'tests - support' }
DRCleanScopesTests >> newCFGWithRootScopeContaining: tempNames [

	| cfg scope |
	cfg := DRMethodControlFlowGraph new.
	scope := DRScope new.
	scope addTempNames: tempNames.
	cfg scope: scope.
	cfg initialBasicBlock initializeScope: scope.
	^ cfg
]

{ #category : 'tests - support' }
DRCleanScopesTests >> store: tempName in: scope [

	^ (DRBCStoreTemporaryVariable
		   operands: { tempName. (DRConstantValue value: 1) })
		  scope: scope;
		  yourself
]

{ #category : 'tests - support' }
DRCleanScopesTests >> load: tempName in: scope [

	^ (DRBCLoadTemporaryVariable operands: { tempName })
		  scope: scope;
		  yourself
]

{ #category : 'tests' }
DRCleanScopesTests >> testRemovesUnusedTemporaryAndStore [

	| cfg scope store |
	cfg := self newCFGWithRootScopeContaining: #( unused ).
	scope := cfg scope.
	store := cfg initialBasicBlock addInstruction:
		         (self store: #unused in: scope).

	self applyOptimisationOn: cfg.

	self assert: scope tempVarNames isEmpty.
	self deny: (cfg instructions includes: store).
]

{ #category : 'tests' }
DRCleanScopesTests >> testKeepsTemporaryThatIsReadAndWritten [

	| cfg scope store load |
	cfg := self newCFGWithRootScopeContaining: #( used ).
	scope := cfg scope.
	store := cfg initialBasicBlock addInstruction:
		         (self store: #used in: scope).
	load := cfg initialBasicBlock addInstruction:
		        (self load: #used in: scope).

	self applyOptimisationOn: cfg.

	self assert: scope tempVarNames equals: #( used ).
	self assert: (cfg instructions includes: store).
	self assert: (cfg instructions includes: load).
]

{ #category : 'tests' }
DRCleanScopesTests >> testRemovesUnusedTemporaryAndInitializationFromInlinedScope [

	| cfg rootScope inlinedScope block inlinedInit store rootInit |
	cfg := self newCFGWithRootScopeContaining: #().
	rootScope := cfg scope.
	block := cfg initialBasicBlock.
	rootInit := rootScope initInstruction.

	inlinedScope := DRScope new.
	inlinedScope outerScope: rootScope.
	inlinedScope addTempNames: #( unused ).
	rootScope addInlinedScope: inlinedScope.
	inlinedInit := block initializeScope: inlinedScope.
	store := block addInstruction: (self store: #unused in: inlinedScope).

	self applyOptimisationOn: cfg.

	self assert: inlinedScope tempVarNames isEmpty.
	self deny: (cfg instructions includes: store).
	self deny: (cfg instructions includes: inlinedInit).
	self assert: (cfg instructions includes: rootInit).
]

{ #category : 'tests' }
DRCleanScopesTests >> testKeepsInitializationForInlinedScopeWithNestedScope [

	| cfg rootScope inlinedScope nestedScope block inlinedInit nestedInit |
	cfg := self newCFGWithRootScopeContaining: #().
	rootScope := cfg scope.
	block := cfg initialBasicBlock.

	inlinedScope := DRScope new.
	inlinedScope outerScope: rootScope.
	rootScope addInlinedScope: inlinedScope.
	inlinedInit := block initializeScope: inlinedScope.

	nestedScope := DRScope new.
	nestedScope outerScope: inlinedScope.
	inlinedScope addInlinedScope: nestedScope.
	nestedInit := block initializeScope: nestedScope.

	self applyOptimisationOn: cfg.

	self assert: (cfg instructions includes: inlinedInit).
	self deny: (cfg instructions includes: nestedInit).
]

{ #category : 'tests' }
DRCleanScopesTests >> testKeepsInitializationForInlinedScopeWithUsedTemporary [

	| cfg rootScope inlinedScope block inlinedInit store load |
	cfg := self newCFGWithRootScopeContaining: #().
	rootScope := cfg scope.
	block := cfg initialBasicBlock.

	inlinedScope := DRScope new.
	inlinedScope outerScope: rootScope.
	inlinedScope addTempNames: #( used ).
	rootScope addInlinedScope: inlinedScope.
	inlinedInit := block initializeScope: inlinedScope.
	store := block addInstruction: (self store: #used in: inlinedScope).
	load := block addInstruction: (self load: #used in: inlinedScope).

	self applyOptimisationOn: cfg.

	self assert: inlinedScope tempVarNames equals: #( used ).
	self assert: (cfg instructions includes: inlinedInit).
	self assert: (cfg instructions includes: store).
	self assert: (cfg instructions includes: load).
]
