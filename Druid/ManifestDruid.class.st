"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestDruid,
	#superclass : #PackageManifest,
	#category : #'Druid-Manifest'
}

{ #category : #'code-critics' }
ManifestDruid class >> ruleUsesAddRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#DRAdd #tryConstantFolding #false)) #'2021-07-15T11:56:26.278621+02:00') #(#(#RGClassDefinition #(#DRAdd)) #'2021-07-15T11:56:55.103421+02:00') )
]
