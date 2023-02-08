Class {
	#name : #DRPrimitiveScenarioCompilationTest,
	#superclass : #DRPrimitiveCompilationTest,
	#category : #'Druid-Tests'
}

{ #category : #'tests-types' }
DRPrimitiveScenarioCompilationTest >> testAsInteger [

	| floatRawValue |
	self compileDruidPrimitive: #primitiveAsInteger.
	self executePrimitiveWithReceiver: 17.0.
	
	floatRawValue := machineSimulator receiverRegisterValue.
	
	self assert: floatRawValue equals: 17
]
