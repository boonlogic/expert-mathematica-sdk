(* Wolfram Language Test file *)

VerificationTest[
	<< NanoREST`,
	Null
	,
	TestID->"Package-load"
]

VerificationTest[
	path=FileNameJoin[{$UserBaseDirectory,"Applications","NanoREST",".BoonLogic.license"}];
	ContainsAll[Keys[nano = OpenNano["test","sample","AuthenticationPath"->path]],{"api-key", "api-tenant", "url", "proxy-server", "instance"}],
	True,
	TestID->"OpenNano-success"
]


VerificationTest[
	ConfigureNano[nano,"uint16",10,MinVals->0,MaxVals->10,PercentVariation->0.07,Weights->1,StreamingWindow->1,NanoAccuracy->0.99]
	,
	Null,
	TestID->"ConfigureNano-uint16-success"
]

VerificationTest[
	ContainsAll[Keys[config=GetConfig[nano]],{"clusterMode", "numericFormat", "features", "percentVariation", "accuracy", "streamingWindowSize", "autoTuning"}],
	True,
	TestID->"GetConfig-success-1"
]

VerificationTest[
	ConfigureNano[nano,ExportString[config,"JSON"]],
	Null,
	TestID->"ConfigureNano-loadconfig-success"
]

VerificationTest[
	ConfigureNano[nano,"int16",10,Weights->{1,2,3,4,5,6,7,8,9,10},AutotunePV->False]
	,
	Null,
	TestID->"ConfigureNano-int16-success"
]

VerificationTest[
	ConfigureNano[nano,"float32",10,MinVals->ConstantArray[0,10],AutotuneRange->False]
	,
	Null,
	TestID->"ConfigureNano-float32-success"
]

VerificationTest[
	ConfigureNano[nano,"float32",10,MaxVals->ConstantArray[10,10],PercentVariation->0.07,AutotuneByFeature->False]
	,
	Null,
	TestID->"ConfigureNano-autotune-overall-success"
]

VerificationTest[
	ConfigureNano[nano,"float32",10,MaxVals->ConstantArray[10,10],PercentVariation->0.07,AutotunePV->False,AutotuneExcludes->{0,9}]
	,
	Null,
	TestID->"ConfigureNano-autotune-excludes-success"
]

VerificationTest[
	ConfigureNano[nano,"uint16",10,MaxVals->ConstantArray[10,10],PercentVariation->0.07]
	,
	Null,
	TestID->"ConfigureNano-autotune-success"
]

VerificationTest[
	ConfigureNano[nano,"uint16",10,ClusterMode->"streaming"],
	Null,
	TestID->"ConfigureNano-streaming-success"
]

VerificationTest[
	list=#["instanceID"]&/@NanoList[nano];
	(nano["instance"]=#;
	CloseNano[nano])&/@list,
	{Null},
	TestID->"Close-all"
]