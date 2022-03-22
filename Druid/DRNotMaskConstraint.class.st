Class {
	#name : #DRNotMaskConstraint,
	#superclass : #DRSimpleConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #testing }
DRNotMaskConstraint >> hasAnyValueWithMask: otherMask [
	
 ^ self includesValue: otherMask
]

{ #category : #arithmetic }
DRNotMaskConstraint >> includesValue: otherConstantValue [

	^ (otherConstantValue anyMask: constantValue) not
]

{ #category : #arithmetic }
DRNotMaskConstraint >> isIncludedIn: otherConstraint [

	^ self error "TODO"
]

{ #category : #arithmetic }
DRNotMaskConstraint >> negated [

	^ DRMaskConstraint withValue: constantValue
]

{ #category : #printing }
DRNotMaskConstraint >> opCode [

	^ 'Not mask'
]
