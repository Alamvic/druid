Class {
	#name : 'DRJITCompileTimeExpressionGenerator',
	#superclass : 'Object',
	#category : 'Druid',
	#package : 'Druid'
}

{ #category : 'visiting' }
DRJITCompileTimeExpressionGenerator >> visitSignedCoercion: aDRSignedCoercion [ 

	"For now just return our expression?"

	^ aDRSignedCoercion operand1 acceptVisitor: self
]
