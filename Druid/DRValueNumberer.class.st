Class {
	#name : 'DRValueNumberer',
	#superclass : 'Object',
	#instVars : [
		'valueNumberList',
		'valueNumberDictionary'
	],
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'initialization' }
DRValueNumberer >> initialize [

	super initialize.

	valueNumberDictionary := Dictionary new
]

{ #category : 'operations' }
DRValueNumberer >> valueNumberOf: instruction [

	" returns the class number of an instruction by recursively checking on the instruction if it fits in any class. "

	| key |
	instruction isCopy ifTrue: [
		^ self valueNumberOf: instruction operand1 ].
	key := instruction isConstant
		       ifTrue: [ instruction ]
		       ifFalse: [
			       | operandValueNumbers |
			       operandValueNumbers := instruction operands collect: [
				                              :operand |
				                              self valueNumberOf: operand ].
			       instruction isCommutative ifTrue: [
				       operandValueNumbers := operandValueNumbers sorted ].
			       { instruction class } , operandValueNumbers ].

	^ valueNumberDictionary
		  at: key
		  ifAbsentPut: valueNumberDictionary size + 1
]
