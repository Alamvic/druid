Extension { #name : 'DRValue' }

{ #category : '*Druid-Cogit' }
DRValue >> asMemoryAddress [

	^ DRAbsoluteMemoryAddress new
		  expression: self;
		  yourself
]
