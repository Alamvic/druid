Class {
	#name : #DRBasicCogInterpreterPrimitivesWithoutParameters,
	#superclass : #CogVMSimulatorLSB,
	#category : #'Druid-Tests-CompilerBuilder'
}

{ #category : #constants }
DRBasicCogInterpreterPrimitivesWithoutParameters class >> basicPrimitiveTable [

	MaxPrimitiveIndex := 8.
	PrimitiveTable := Array new: MaxPrimitiveIndex + 1.
	self primitivesClass
		table: PrimitiveTable
		from: #(	"Integer Primitives (0-19)"
			(0 0 "a.k.a. primitiveFail but faster because primitiveFail won't even be called")
			(1 primitiveAsCharacter)
			(2 primitiveAsFloat)
			(3 primitiveSize)
			(4 primitiveFullGC)
			(5 primitiveIncrementalGC)
			(6 primitiveGrowMemoryByAtLeast)
			(7 primitiveBytesLeft)
			(8 primitiveGetenv)
		).
	^ PrimitiveTable
]
