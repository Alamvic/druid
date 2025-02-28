Class {
	#name : 'DRConstantPhiInsertionTest',
	#superclass : 'DRSSAPRETest',
	#category : 'Druid-Tests-Optimizations',
	#package : 'Druid-Tests',
	#tag : 'Optimizations'
}

{ #category : 'tests' }
DRConstantPhiInsertionTest >> testPhiInsertionInConditionalCase1 [
	" 
	  T1  →   T2 = e   →   T3   
          \_________/ 
   "

	| cfg b3 b1 b2 instruction |
	
	cfg := self setUpCFG: 3.
	b1 := cfg b1.
	b2 := cfg b2.
	b3 := cfg b3.
   instruction := b2 copy: 3.
	b2 removePredecessor: cfg b1.
	b1 jumpIfTrueTo: b2 ifFalseTo: b3.
	
	optimisation insertPhi: cfg.
	
	self assert: b3 hasPhiFunctions equals: true.
	
	
	
	
]

{ #category : 'tests' }
DRConstantPhiInsertionTest >> testPhiInsertionInConditionalCase2 [
	" 
       /------------------\  
	 B5 →  B1 = e  →  B2  → B4
                 \→  B3  /

   "

	| cfg b3 b1 b2 b4 instruction1 b5 |
	cfg := self setUpCFGWithConditional.
	b1 := cfg b1.
	b2 := cfg b2.
	b3 := cfg b3.
	b4 := cfg b4.
	b5 := cfg newBasicBlock.
   cfg initialBasicBlock jumpTo: b5.
	b5 jumpIfTrueTo: b1 ifFalseTo: b4.
	instruction1 := b1 copy: 3.

	optimisation insertPhi: cfg.

	self assert: b4 hasPhiFunctions equals: true
]

{ #category : 'tests' }
DRConstantPhiInsertionTest >> testPhiInsertionInConditionalCase3 [
	" 
       /------------------\  
	 B5 →  B1 = e  →  B2 = e  → B4
                  \→  B3    /

   "

	| cfg b3 b1 b2 b4 instruction1 instruction2 b5 |
	cfg := self setUpCFGWithConditional.
	b1 := cfg b1.
	b2 := cfg b2.
	b3 := cfg b3.
	b4 := cfg b4.
	b5 := cfg newBasicBlock.
   cfg initialBasicBlock jumpTo: b5.
	instruction1 := b1 copy: 3.
	instruction2 := b2 copy: 3.
	b5 jumpIfTrueTo: b1 ifFalseTo: b4.

	optimisation insertPhi: cfg.

	self assert: b4 hasPhiFunctions equals: true.
	self assert: b4 instructions size equals: 3.
]

{ #category : 'tests' }
DRConstantPhiInsertionTest >> testPhiInsertionInConditionalCase4 [
	" 
       /------------------\  
	 B5 →  B1 = e  →  B2 = e  → B4 = p
                  \→  B3    /

   "

	| cfg b3 b1 b2 b4 instruction1 instruction2 b5 instruction3 |
	cfg := self setUpCFGWithConditional.
	b1 := cfg b1.
	b2 := cfg b2.
	b3 := cfg b3.
	b4 := cfg b4.
	b5 := cfg newBasicBlock.
   cfg initialBasicBlock jumpTo: b5.
	instruction1 := b1 copy: 3.
	instruction2 := b2 copy: 3.
	instruction3 := b3 copy: 1.
	b5 jumpIfTrueTo: b1 ifFalseTo: b4.

	optimisation insertPhi: cfg.

	self assert: b4 hasPhiFunctions equals: true.
	self assert: b4 instructions size equals: 4.
]

{ #category : 'tests' }
DRConstantPhiInsertionTest >> testPhiInsertionInConditionalCase5 [
"
T1  →  T2 = e  →   T4 = e
    \→  T3 = e /
⇒ 
T1  → T2 = e  →  T5 = Φ, T4 = e
    \→  T3 = e /
"

	| cfg instruction1 instruction2 instruction3 instruction4 |
	cfg := self setUpCFGWithConditional: (instructionFactory jumpIf:
			        (DRPhysicalGeneralPurposeRegister name: 'Parameter')).
	instruction1 := cfg b2 copy: 3.
	instruction2 := cfg b3 copy: 3.
	instruction3 := cfg b4 copy: 3.
	

	optimisation insertPhi: cfg.

	self assert: cfg b2 first isCopy.
	self assert: cfg b3 first isCopy.
	self assert: cfg b4 hasPhiFunctions. 
]
