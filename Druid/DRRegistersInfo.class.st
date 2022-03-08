Class {
	#name : #DRRegistersInfo,
	#superclass : #Object,
	#instVars : [
		'constraints'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #'instance creation' }
DRRegistersInfo class >> new: constraints [

	^ self new
		  constraints: constraints;
		  yourself
]

{ #category : #adding }
DRRegistersInfo >> addConstraint: aConstraint [ 
	
	constraints add: aConstraint
]

{ #category : #accessing }
DRRegistersInfo >> constraints [

	^ constraints
]

{ #category : #accessing }
DRRegistersInfo >> constraints: anObject [

	constraints := anObject
]

{ #category : #initialization }
DRRegistersInfo >> initialize [ 

	constraints := Dictionary new.
]

{ #category : #visiting }
DRRegistersInfo >> visitConstant: aDRConstantValue [ 
	
	^ DRConstantRegisterConstraint new: aDRConstantValue value 
]

{ #category : #visiting }
DRRegistersInfo >> visitCopy: aDRCopy [ 
	
	| operandConstraint |
	operandConstraint := aDRCopy operands first acceptVisitor: self.
	self addConstraint: aDRCopy result -> operandConstraint
]

{ #category : #visiting }
DRRegistersInfo >> visitJump: aDRJump [ 

	"Simple jumps do not have type constraints"
]

{ #category : #visiting }
DRRegistersInfo >> visitReturn: aDRReturn [

	"Returns do not have type constraints"

	
]

{ #category : #visiting }
DRRegistersInfo >> visitTest: aDRTest [ 
	self shouldBeImplemented.
]
