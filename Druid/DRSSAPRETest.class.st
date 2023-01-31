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
DRSSAPRETest >> testFullRedundancyCaseWith3ConstExpressInSingleBlock [
	"case B: full redundancy
	T1 = 3
	T2 = 3
	T3 = 3
	
	⇒
	
	T1 = 3
	T2 = T1
	T3 = T1"

	| cfg instruction1 instruction2 instruction3 |
	cfg := self setUpCFG: 1.
	instruction1 := cfg b1 copy: 3.
	instruction2 := cfg b1 copy: 3.
	instruction3 := cfg b1 copy: 3.


	optimisation applyTo: cfg.

	self assert: cfg b1 third isCopy.
	self assert: cfg b1 third operand1 equals: instruction1
]

{ #category : #tests }
DRSSAPRETest >> testFullRedundancyCaseWithAlternateConstExpressInLinearCF [
	"case C: full redundancy linear CF with alternate instructions
		T1 = e → T2 = e2 → T3 = e → T4= e2"

	| cfg instruction1 instruction2 instruction3 |
	
	cfg := self setUpCFG: 3.
	instruction1 := cfg b1 copy: 1.
	instruction2 := cfg b2 copy: 3.
	instruction3 := cfg b3 copy: 1.

	optimisation applyTo: cfg.
	
	self assert: cfg b1 first isCopy.
	self assert: cfg b2 first isCopy.
	self assert: cfg b3 first operand1 equals: instruction1.

]

{ #category : #tests }
DRSSAPRETest >> testFullRedundancyCaseWithConstantInConditionCF [
	"case E: full redundancy condition
T1 = e → T2 = e → T4
         \→  T3 = e /
⇒ 

T1 = e → T2 = T1    → T4
           \→  T3 = T1/
"

	| cfg instruction1 instruction2 instruction3 |
	cfg := self setUpCFGWithConditional: (instructionFactory jumpIf:
			        (DRPhysicalGeneralPurposeRegister name: 'Parameter')).
	instruction1 := cfg b1 copy: 6.
	instruction2 := cfg b2 copy: 6.
	instruction3 := cfg b3 copy: 6.
	
	optimisation applyTo: cfg.
	
	self assert: cfg b2 first isCopy.
	self assert: cfg b3 first isCopy.
	self assert: cfg b2 first operand1 equals: instruction1.
	self assert: cfg b3 first operand1 equals: instruction1.
]

{ #category : #tests }
DRSSAPRETest >> testFullRedundancyCaseWithConstantInSingleBlock [
	"case B: full redundancy
	T1 = 3
	T2 = 3
	
	⇒
	
	T1 = 3
	T2 = T1"

	| cfg instruction1 instruction2 |
	cfg := self setUpCFG: 1.
	instruction1 := cfg b1 copy: 3.
	instruction2 := cfg b1 copy: 3.

	optimisation applyTo: cfg.

	self assert: cfg b1 second isCopy.
	self assert: cfg b1 second operand1 equals: instruction1.
]

{ #category : #tests }
DRSSAPRETest >> testFullRedundancyCaseWithMultipleConstExpressInLinearCF [
	"case C: full redundancy linear CF with multiple instructions
	T1 = e 
	T2 = e2
	→ 
	T3 = e
	T4 = e2
		⇒    
	T1 = e 
	T2 = e2
	→ 
	T3 = T1
	T4 = T2 "

	| cfg instruction1 instruction2 instruction3 instruction4 |
	
	cfg := self setUpCFG: 2.
	instruction1 := cfg b1 copy: 1.
	instruction2 := cfg b1 copy: 3.
	instruction3 := cfg b2 copy: 1.
	instruction4 := cfg b2 copy: 3.
	
	optimisation applyTo: cfg.
	
	self assert: cfg b2 first isCopy.
	self assert: cfg b2 second isCopy.
	self assert: cfg b2 first operand1 equals: instruction1.
	self assert: cfg b2 second operand1 equals: instruction2.
]

{ #category : #tests }
DRSSAPRETest >> testFullRedundancyCaseWithSingleConstExpressInLinearCF [
	"case C: full redundancy linear CF with single instruction
	T1 = e → T2 = e
	⇒ 
	T1 = e → T2 = T1 "

	| cfg instruction1 instruction2 |
	
	cfg := self setUpCFG: 2.
	
	instruction1 := cfg b1 copy: 3.
	instruction2 := cfg b2 copy: 3.
	
	optimisation applyTo: cfg.
	
	self assert: cfg b2 first isCopy.
	self assert: cfg b2 first operand1 equals: instruction1.
	
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
DRSSAPRETest >> testNoRedundancyCaseWithConstantInDiffBlock [
	"case A: no redundancy
	T1 = 3
	->
	T2 = 2
	
	⇒ do nothing"

	| cfg instruction1 instruction2 |
	cfg := self setUpCFG: 2.
	instruction1 := cfg b1 copy: 3.
	instruction2 := cfg b2 copy: 2.

	optimisation applyTo: cfg.

	self assert: cfg b1 first isCopy.
	self assert: cfg b1 first operand1 value equals: 3.
	
	self assert: cfg b2 first isCopy.
	self assert: cfg b2 first operand1 value equals: 2.
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
DRSSAPRETest >> testRedundancyWithSameConstIn124BlockInConditionCF [
" 
T1 = e → T2  = e → T4 = e
         \→  T3  /
⇒ 

T1 = e → T2 = T1  → T4 = T1
           \→  T3 /
"
	| cfg instruction1 instruction2 instruction3 |
	cfg := self setUpCFGWithConditional: (instructionFactory jumpIf:
			        (DRPhysicalGeneralPurposeRegister name: 'Parameter')).
	instruction1 := cfg b1 copy: 3.
	instruction2 := cfg b2 copy: 3.
	instruction3 := cfg b4 copy: 3.
	
	optimisation applyTo: cfg.
	
	self assert: cfg b2 first operand1 isCopy.
	self assert: cfg b4 first operand1 isCopy.
	self assert: cfg b2 first operand1 equals: instruction1.
	self assert: cfg b4 first operand1 equals: instruction1.
]

{ #category : #tests }
DRSSAPRETest >> testRedundancyWithSameConstIn1and4BlockInConditionCF [
" 
T1 = e → T2  → T4 = e
         \→  T3  /
⇒ 

T1 = e → T2   → T4 = T1
           \→  T3 /
"
		
			
	| cfg instruction1 instruction2 |
	cfg := self setUpCFGWithConditional: (instructionFactory jumpIf:
			        (DRPhysicalGeneralPurposeRegister name: 'Parameter')).
	instruction1 := cfg b1 copy: 3.
	instruction2 := cfg b4 copy: 3.
	
	optimisation applyTo: cfg.
	
	self assert: cfg b4 first operand1 isCopy.
	self assert: cfg b4 first operand1 equals: instruction1.
]
