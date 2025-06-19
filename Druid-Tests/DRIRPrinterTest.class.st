Class {
	#name : 'DRIRPrinterTest',
	#superclass : 'DRIRTest',
	#category : 'Druid-Tests-IR',
	#package : 'Druid-Tests',
	#tag : 'IR'
}

{ #category : 'tests' }
DRIRPrinterTest >> testPrintConditionalCFG [

	| printer cfg t0 instructionFactory condition |
	cfg := self setUpCFGWithConditionalWithPhi.

	instructionFactory := cfg instructionFactory.
	condition := instructionFactory less: 1 than: 2.
	cfg b1 addInstructionFirst: condition.
	cfg b1 lastInstruction operands: {
			DREqualsThanComparison new.
			condition.
			true asDRValue }.

	t0 := cfg b1 copy: 5.
	cfg b1 multiply: 3 by: t0.

	printer := DRIRPrinter for: cfg.

	self assert: printer asString equals: '[[b0]]

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
'
]

{ #category : 'tests' }
DRIRPrinterTest >> testPrintLinearCFG [

	| printer cfg t0 |
	cfg := self setUpCFG: 3.
	t0 := cfg b1 copy: 5.
	cfg b1 multiply: 3 by: t0.

	printer := DRIRPrinter for: cfg.

	self assert: printer asString equals: '[[b0]]

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
'
]
