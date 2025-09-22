Class {
	#name : 'DRPiZ3DeadBranchEliminationTest',
	#superclass : 'DRPiNodesTest',
	#category : 'Druid-Tests-Optimizations',
	#package : 'Druid-Tests',
	#tag : 'Optimizations'
}

{ #category : 'tests' }
DRPiZ3DeadBranchEliminationTest >> testAdditionBetweenVariables [

	"                /->--------/----------/------------>-|b_end1|
	                /          /                     /-->-|b_end2|
	 |b0| -> |b_if1| -> |b_if2| -> |b_if3| -> |b_if4| --> |b_end_unreachable|
	"

	| cfg b_end2 b_end_unreachable b_if4 blocks |
	
	cfg := (DRIRParser for: '[[b0]]
b_if1:
	x := LoadFramePointer()
	y := LoadFramePointer()
	z := LoadFramePointer()
	_ := If(GreaterThan, x, z; b_if2, b_end1)
b_if2:
	_ := If(GreaterThan, y, z; b_if3, b_end1)
b_if3:
	_ := If(GreaterThan, z, 0; b_if4, b_end1)
b_if4:
	sum := Add(x, y)
	_ := If(GreaterThan, sum, z; b_end2, b_end_unreachable)

b_end1:
	_ := Store(8, 2, 777)
b_end2:
	_ := Store(8, 2, 888)
b_end_unreachable:
	_ := Store(8, 2, 999)

b0:
	_ := Jump(b_if1)
') parse.

	blocks := cfg blocks.
	b_end_unreachable := blocks at: 7.
	b_end2 := blocks at: 6.
	b_if4 := blocks at: 4.

	self optimize: cfg with: { DRPiZ3DeadBranchElimination }.
	
	self assert: (cfg blocks includes: b_end_unreachable) not.
	self assert: b_if4 endInstruction isJump.
	self assert: b_if4 endInstruction isConditionalBranch not.
	self assert: b_if4 endInstruction target equals: b_end2.

]

{ #category : 'tests' }
DRPiZ3DeadBranchEliminationTest >> testDetectRedundantIfConstantWithConstant [

	"
	               /->-|b_end|
	 |b0| -> |b_if| -> |b_end_unreachable|
	"

	| cfg b_end b_if blocks b_end_unreachable |
	
	cfg := (DRIRParser for: '[[b0]]
b_if:
	t1 := Copy(4)
	_ := If(LessThan, t1, 3; b_end_unreachable, b_end)

b_end:
	_ := Store(8, 2, 777)
b_end_unreachable:
	_ := Store(8, 2, 888)

b0:
	_ := Jump(b_if)
') parse.
	
	blocks := cfg blocks.
	b_end_unreachable := blocks at: 3.
	b_end := blocks at: 2.
	b_if := blocks at: 1.

	self optimize: cfg with: { DRPiZ3DeadBranchElimination }.
	
	self assert: (cfg blocks includes: b_end_unreachable) not.
	self assert: b_if endInstruction isJump.
	self assert: b_if endInstruction isConditionalBranch not.
	self assert: b_if endInstruction target equals: b_end.

]

{ #category : 'tests' }
DRPiZ3DeadBranchEliminationTest >> testDetectRedundantIfVarWithConstants [

	"                /->--------->-|b_end0|
	                /          /->-|b_end1|
	 |b0| -> |b_if1| -> |b_if2| -> |b_end_unreachable|
	"

	| cfg b_end1 b_end_unreachable b_if2 blocks |
	
	cfg := (DRIRParser for: '[[b0]]
b_if1:
	x := LoadFramePointer()
	_ := If(LessThan, x, 3; b_if2, b_end0)
b_if2:
	_ := If(LessThan, x, 5; b_end1, b_end_unreachable)

b_end0:
	_ := Store(8, 2, 777)
b_end1:
	_ := Store(8, 2, 888)
b_end_unreachable:
	_ := Store(8, 2, 999)

b0:
	_ := Jump(b_if1)
') parse.

	blocks := cfg blocks.
	b_end_unreachable := blocks at: 5.
	b_end1 := blocks at: 4.
	b_if2 := blocks at: 2.

	self optimize: cfg with: { DRPiZ3DeadBranchElimination }.
	
	self assert: (cfg blocks includes: b_end_unreachable) not.
	self assert: b_if2 endInstruction isJump.
	self assert: b_if2 endInstruction isConditionalBranch not.
	self assert: b_if2 endInstruction target equals: b_end1.

]

{ #category : 'tests' }
DRPiZ3DeadBranchEliminationTest >> testUpperBoundsCheckElimination [

	"                
	                           /->-|b_error|
	 |b0| -> |b_i| -> |b_while| -> |b_access| -> |b_end|
	                  \-<-------------------/
	"

	| cfg blocks b_error b_access b_while |

	cfg := (DRIRParser for: '[[b0]]
b_i:
	len := LoadFramePointer()
	i0 := Copy(0)
	_ := Jump(b_while)
b_while:[b_i, b_access]
	i1 := phi(i0, i2)
	_ := If(LessThan, i1, len; b_access, b_error)

b_access:
	x := LoadFramePointer()
	i2 := Add(i1, 1)
	_ := If(LessThan, i2, len; b_while, b_end)

b_end:
	_ := Store(8, 2, 777)
b_error:
	_ := Store(8, 2, 999)

b0:
	_ := Jump(b_i)
') parse.

	blocks := cfg blocks.
	b_error := blocks at: 5.
	b_access := blocks at: 3.
	b_while := blocks at: 2.

	self optimize: cfg with: { DRPiZ3DeadBranchElimination }.
	
	self assert: (cfg blocks includes: b_error) not.
	self assert: b_while endInstruction isJump.
	self assert: b_while endInstruction isConditionalBranch not.
	self assert: b_while endInstruction target equals: b_access.

]
