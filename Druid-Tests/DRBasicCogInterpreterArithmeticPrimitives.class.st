"
Implements an Interpreter with very few arithmetic primitives. None of the primitives specify parameters.
"
Class {
	#name : 'DRBasicCogInterpreterArithmeticPrimitives',
	#superclass : 'CogVMSimulatorLSB',
	#category : 'Druid-Tests-CompilerBuilder',
	#package : 'Druid-Tests',
	#tag : 'CompilerBuilder'
}

{ #category : 'constants' }
DRBasicCogInterpreterArithmeticPrimitives class >> basicPrimitiveTable [

	MaxPrimitiveIndex := 10.
	PrimitiveTable := Array new: MaxPrimitiveIndex + 1.
	self primitivesClass
		table: PrimitiveTable
		from: #(	"Integer Primitives (0-19)"
			(0 0 "a.k.a. primitiveFail but faster because primitiveFail won't even be called")
			(1 primitiveAdd)
			(2 primitiveSubtract)
			(3 primitiveLessThan)
			(4 primitiveGreaterThan)
			(5 primitiveLessOrEqual)
			(6 primitiveGreaterOrEqual)
			(7 primitiveEqual)
			(8 primitiveNotEqual)
			(9 primitiveMultiply)
			(10 primitiveDivide)
		).
	^ PrimitiveTable
]

{ #category : 'constants' }
DRBasicCogInterpreterArithmeticPrimitives class >> primitivesClass [

	^ self
]
