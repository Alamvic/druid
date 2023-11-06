Class {
	#name : #DRNoRegister,
	#superclass : #DRValue,
	#category : #'Druid-IR'
}

{ #category : #testing }
DRNoRegister >> isNoResult [
	
	^ true
]

{ #category : #accessing }
DRNoRegister >> name [
	
	^ '_'
]

{ #category : #'cogit-generation' }
DRNoRegister >> rtlOperandQualifier [

	^ 'Null:' ":("
]
