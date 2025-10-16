Class {
	#name : 'ManifestDruidCogit',
	#superclass : 'PackageManifest',
	#category : 'Druid-Cogit-Manifest',
	#package : 'Druid-Cogit',
	#tag : 'Manifest'
}

{ #category : 'accessing' }
ManifestDruidCogit class >> formatter [

	^ DRCogitFormatter new
]
