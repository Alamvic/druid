Class {
	#name : #DRSSAPRETest,
	#superclass : #DROptimisationTest,
	#instVars : [
		'instructionFactory'
	],
	#category : #'Druid-Tests'
}

{ #category : #running }
DRSSAPRETest >> setUp [

	super setUp.
	instructionFactory := DRInstructionFactory new.
	optimisation := DRSSAPRE new.
	
]

{ #category : #tests }
DRSSAPRETest >> testFullRedundancyInsertionPhiInConditionCF [ 
"
T1  → T2 = e  → T4 = e
      \→  T3 = e /
⇒ 
T1  → T2 = e  → T4 = e
      \→  T3 = e /
"
	
| cfg instruction1 instruction2 instruction3 |

	cfg := self setUpCFGWithConditional: (instructionFactory jumpIf:
			        (DRPhysicalGeneralPurposeRegister name: 'Parameter')).
	instruction1 := cfg b2 copy: 3.
	instruction2 := cfg b3 copy: 3.
	instruction3 := cfg b4 copy: 3.
    
	optimisation insertPhi: cfg.
   
	
	self assert: cfg b4 hasPhiFunctions equals: true.
	self assert: (cfg b4 instructions at: 1) isPhiFunction. 
	self assert: (cfg b4 instructions at: 2) operand1 value equals: 3.
]

{ #category : #tests }
DRSSAPRETest >> testFulllRedundancyRenameInConditionCF [
	"
T1  → T2 = e        → T5 = Φ, T4 = e
      \→  T3 = e /
⇒ 
T1  → T2 = e  		→ T4 = T5
      \→  T3 = e /
"

	| cfg instruction1 instruction2 instruction3 |
	cfg := self setUpCFGWithConditional: (instructionFactory jumpIf:
			        (DRPhysicalGeneralPurposeRegister name: 'Parameter')).
	instruction1 := cfg b2 copy: 3.
	instruction2 := cfg b3 copy: 3.
	instruction3 := cfg b4 copy: 3.

	optimisation insertPhi: cfg.
	optimisation renamePhi: cfg.


	self assert: (cfg b4 instructions at: 2) operand1 equals: (cfg b4 instructions at: 1)
]

{ #category : #tests }
DRSSAPRETest >> testPartialRedundancyWithSameConstIn234BlockInConditionCF [
	"
T1  → T2 = e  → T4 = e
      \→  T3 = e /
⇒ 
T1  → T2 = e  → T4 = e
      \→  T3 = e /
"

	| cfg instruction1 instruction2 instruction3 |
	cfg := self setUpCFGWithConditional: (instructionFactory jumpIf:
			        (DRPhysicalGeneralPurposeRegister name: 'Parameter')).
	instruction1 := cfg b2 copy: 3.
	instruction2 := cfg b3 copy: 3.
	instruction3 := cfg b4 copy: 3.

	optimisation insertPhi: cfg.
	optimisation renamePhi: cfg.


	self assert: (cfg b4 instructions at: 2) operand1 equals: (cfg b4 instructions at: 1)
]

{ #category : #tests }
DRSSAPRETest >> testPhiInsertionOnDFcase1 [
	" 
	  T1  →   T2 = e   →   T3   
          \_________/ 
   "

	| cfg b3 b1 b2 instruction |
	cfg := DRControlFlowGraph new.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b1 jumpIfTrueTo: b2 ifFalseTo: b3.
	b2 jumpTo: b3.
	instruction := cfg b2 copy: 3.
	
	optimisation insertPhi: cfg.
	
]
