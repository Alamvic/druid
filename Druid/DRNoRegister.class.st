Class {
	#name : 'DRNoRegister',
	#superclass : 'DRRegister',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'testing' }
DRNoRegister >> isNoResult [
	
	^ true
]

{ #category : 'accessing' }
DRNoRegister >> prettyPrint [
	
	^ '_'
]

{ #category : 'cogit-generation' }
DRNoRegister >> rtlOperandQualifier [

	^ 'Null:' ":("
]
