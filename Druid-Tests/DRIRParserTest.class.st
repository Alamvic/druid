Class {
	#name : 'DRIRParserTest',
	#superclass : 'DRIRTest',
	#category : 'Druid-Tests-IR',
	#package : 'Druid-Tests',
	#tag : 'IR'
}

{ #category : 'tests' }
DRIRParserTest >> testParseConditionalCFG [

	| printer cfg src parser |
	src := 	'[[b0]]

b1:
	t3 := LessThan(1, 2)
	t4 := :=(5)
	t5 := MUL(3, t4)
	_ := If(EqualsThan, t3, true; b2, b3)
b2:
	t0 := :=(1)
	_ := Jump(b4)
b3:
	t1 := :=(2)
	_ := Jump(b4)
b4:[b2, b3]
	t2 := Ø(t0, t1)
	_ := Noop()
b0:
	_ := Jump(b1)
'.

	parser := DRIRParser for: src.
	cfg := parser parse.
	printer := DRIRPrinter for: cfg.

	self assert: printer asString equals: '[[b0]]

b1:
	t0 := LessThan(1, 2)
	t1 := :=(5)
	t2 := MUL(3, t1)
	t3 := If(EqualsThan, t0, true; b2, b3)
b2:
	t4 := :=(1)
	_ := Jump(b4)
b3:
	t6 := :=(2)
	_ := Jump(b4)
b4:
	t8 := Ø(t4, t6)
	_ := Noop()
b0:
	_ := Jump(b1)
'
]

{ #category : 'tests' }
DRIRParserTest >> testParseCyclicCFG [

	| printer cfg src parser |
	src := 	'[[b0]]

b1:
	t1 := Ø(t0, t2)
	_ := If(LessThan, t1, 10; b2, b3)
b2:
	t2 := ADD(1, t1)
	_ := Jump(b1)
b3:
	_ := Return(t1)
b0:
	t0 := :=(1)
	_ := Jump(b1)
'.

	parser := DRIRParser for: src.
	cfg := parser parse.
	printer := DRIRPrinter for: cfg.

	self assert: printer asString equals: '[[b0]]

b1:
	t0 := Ø(t5, t2)
	t1 := If(LessThan, t0, 10; b2, b3)
b2:
	t2 := ADD(1, t0)
	_ := Jump(b1)
b3:
	t4 := Return(t0)
b0:
	t5 := :=(1)
	_ := Jump(b1)
'.
]

{ #category : 'tests' }
DRIRParserTest >> testParseLinearCFG [

	| printer cfg src parser |
	src := '[[b0]]

b1:
	t0 := :=(5)
	t1 := MUL(3, t0)
	_ := Jump(b2)
b2:
	_ := Jump(b3)
b3:
	_ := Jump(b4)
b4:
	_ := Noop()
b0:
	_ := Jump(b1)
'.

	parser := DRIRParser for: src.
	cfg := parser parse.
	printer := DRIRPrinter for: cfg.

	self assert: printer asString equals: src
]
