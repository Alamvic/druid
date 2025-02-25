Class {
	#name : 'DRNoMultiplyOverflow',
	#superclass : 'DRNoOverflow',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'testing' }
DRNoMultiplyOverflow >> isNoMultiplyOverflow [
	
	^ true
]

{ #category : 'transforming' }
DRNoMultiplyOverflow >> negated [

	^ DRMultiplyOverflow new
]
