"
A LiveRange is an object that represents the lifetime or duration of a specific variable within a program. It specifies the interval during which the variable is live or has a value. A live range consists of a start point and an end point in the program's code, denoting the range where the variable is used or modified.

## Important methods

The `overlaps:` method takes another Live Range as a parameter and checks whether the order interval of the receiver intersects with the order interval of the second LiveRange. The order interval represents the sequential order of the program points where the variable is live. If the intersection of the two order intervals is not empty, meaning there is a common range of program points where both variables are live, `overlaps:` will answer true. Otherwise, it will return false.

Internal Representation and Key Implementation Points.

    Instance Variables
	start:		<Object>
	stop:		<Object>


    Implementation Points
"
Class {
	#name : 'DRLiveRange',
	#superclass : 'Object',
	#instVars : [
		'start',
		'stop'
	],
	#category : 'Druid-LinearScanRegisterAllocation',
	#package : 'Druid',
	#tag : 'LinearScanRegisterAllocation'
}

{ #category : 'testing' }
DRLiveRange >> isActiveAt: aPosition [
	
	"We consider that the stop of the live range is open at the end.
	"
	^ aPosition between: self start order and: self stop order - 1
]

{ #category : 'accessing' }
DRLiveRange >> orderInterval [

	^ self start order to: self stop order - 1
]

{ #category : 'testing' }
DRLiveRange >> overlaps: aDRLiveRange [ 

	^ (self orderInterval intersection: aDRLiveRange orderInterval) notEmpty
]

{ #category : 'printing' }
DRLiveRange >> printOn: aStream [

	super printOn: aStream.
	aStream << '['.
	aStream << self start order asString.
	aStream << ', '.
	aStream << self stop order asString.
	aStream << '['.
]

{ #category : 'accessing' }
DRLiveRange >> start [

	^ start
]

{ #category : 'accessing' }
DRLiveRange >> start: anObject [

	start := anObject
]

{ #category : 'accessing' }
DRLiveRange >> stop [

	^ stop
]

{ #category : 'accessing' }
DRLiveRange >> stop: anObject [

	stop := anObject
]
