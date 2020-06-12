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
	GetConfig[nano],
	Null,
	{NanoError::return},
	TestID->"GetConfig-failure"
]

VerificationTest[
	ConfigureNano[nano,"uint16",10,MinVals->0,MaxVals->ConstantArray[10,9],PercentVariation->0.07,Weights->1,StreamingWindow->1,NanoAccuracy->0.99]
	,
	Null,
	{NanoError::length},
	TestID->"ConfigureNano-length-failure"
]

VerificationTest[
	ConfigureNano[nano,"uint16",10,MinVals->1,MaxVals->10,PercentVariation->0.07,Weights->1,StreamingWindow->1,NanoAccuracy->0.99]
	,
	Null,
	{NanoError::return},
	TestID->"ConfigureNano-min-failure"
]

VerificationTest[
	ConfigureNano[nano,nano],
	Null,
	{NanoError::return},
	TestID->"ConfigureNano-loadconfig-failure"
]

VerificationTest[
	AutotuneConfig[nano],
	Null,
	{NanoError::return},
	TestID->"AutotuneConfig-failure-no-data"
]

VerificationTest[
	list=#["instanceID"]&/@NanoList[nano];
	(nano["instance"]=#;
	CloseNano[nano])&/@list,
	{Null},
	TestID->"Close-all"
]