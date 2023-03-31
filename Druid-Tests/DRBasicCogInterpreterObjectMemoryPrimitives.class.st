Class {
	#name : #DRBasicCogInterpreterObjectMemoryPrimitives,
	#superclass : #CogVMSimulatorLSB,
	#category : #'Druid-Tests-CompilerBuilder'
}

{ #category : #constants }
DRBasicCogInterpreterObjectMemoryPrimitives class >> basicPrimitiveTable [

	MaxPrimitiveIndex := 7.
	PrimitiveTable := Array new: MaxPrimitiveIndex + 1.
	self primitivesClass
		table: PrimitiveTable
		from: #(	"Integer Primitives (0-19)"
			(0 0 "a.k.a. primitiveFail but faster because primitiveFail won't even be called")
			(1 primitiveSize)
			(2 primitiveFullGC)
			(3 primitiveIncrementalGC)
			(4 primitiveGrowMemoryByAtLeast)
			(5 primitiveBytesLeft)
			(6 primitiveGetenv)
			(7 primitivePin)
		).
	^ PrimitiveTable
]
