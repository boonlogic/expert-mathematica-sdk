(* Wolfram Language Test file *)

VerificationTest[
	<< NanoREST`,
	Null
	,
	TestID->"Package-load"
]

VerificationTest[
	path=FileNameJoin[{$UserBaseDirectory,"Applications","NanoREST",".BoonLogic.license"}];
	ContainsAll[Keys[nano = OpenNano["test", "sample", 
								"AuthenticationPath"->path]
					],{"api-key", "api-tenant", "url", "proxy-server", "instance"}],
	True,
	TestID->"OpenNano-specify-user"
]



VerificationTest[
	ConfigureNano[nano,"uint16",10,MinVals->0,MaxVals->10,PercentVariation->0.07,Weights->1,StreamingWindow->1,NanoAccuracy->0.99]
	,
	Null,
	TestID->"ConfigureNano-uint16-success"
]

VerificationTest[
	LoadData[nano,Import[FileNameJoin[{$UserBaseDirectory,"Applications","NanoREST","ExampleData.csv"}],"CSV"]],
	Null,
	TestID->"LoadData-success"
]

VerificationTest[
	GetBufferStatus[nano]["totalBytesWritten"],
	400,
	TestID->"GetBufferStatus-success"
]

VerificationTest[
	AutotuneConfig[nano],
	Null,
	TestID->"AutotuneConfig-just-range-success"
]

VerificationTest[
	ConfigureNano[nano,"float32",10,MinVals->ConstantArray[0,10],AutotuneRange->False]
	,
	Null,
	TestID->"ConfigureNano-float32-success"
]

VerificationTest[
	AutotuneConfig[nano],
	Null,
	TestID->"AutotuneConfig-just-pv-success"
]

VerificationTest[
	ConfigureNano[nano,"float32",10,MaxVals->ConstantArray[10,10],PercentVariation->0.07,AutotuneByFeature->False]
	,
	Null,
	TestID->"ConfigureNano-autotune-overall-success"
]

VerificationTest[
	AutotuneConfig[nano],
	Null,
	TestID->"AutotuneConfig-overall-success"
]

VerificationTest[
	ConfigureNano[nano,"float32",10,MaxVals->ConstantArray[10,10],PercentVariation->0.07,AutotunePV->False,AutotuneExcludes->{0,9}]
	,
	Null,
	TestID->"ConfigureNano-autotune-excludes-success"
]

VerificationTest[
	AutotuneConfig[nano],
	Null,
	TestID->"AutotuneConfig-byfeature-exclusions-success"
]

VerificationTest[
	ConfigureNano[nano,"uint16",10,MaxVals->ConstantArray[10,10],PercentVariation->0.07]
	,
	Null,
	TestID->"ConfigureNano-autotune-success"
]

VerificationTest[
	AutotuneConfig[nano],
	Null,
	TestID->"AutotuneConfig-all-success"
]

VerificationTest[
	LearningQ[nano],
	True,
	TestID->"LearningQ-True"
	
]

VerificationTest[
	LoadData[nano,Import[FileNameJoin[{$UserBaseDirectory,"Applications","NanoREST","ExampleData.csv"}],"CSV"],AppendData->True],
	Null,
	TestID->"LoadData-append-success"
]

VerificationTest[
	Sort[GetNanoStatus[nano]],
	<|"numClusters" -> 1, "clusterGrowth" -> {0}, "clusterSizes" -> {0}, "frequencyIndexes" -> {0}, "distanceIndexes" -> {0}, "anomalyIndexes" -> {1000}, "PCA" -> {{0, 0, 0}}|>,
	TestID->"GetNanoStatus-zero cluster"
]

VerificationTest[
	GetBufferStatus[nano]["totalBytesWritten"],
	800,
	TestID->"GetBufferStatus-success-1"
]

VerificationTest[
	RunNano[nano],
	Null,
	TestID->"RunNano-success"
]

VerificationTest[
	ContainsAll[Keys[RunNano[nano,Results->{ID,RI}]],{"ID","RI"}],
	True,
	TestID->"RunNano-results-success"
]

VerificationTest[
	numClusts=GetNanoStatus[nano]["numClusters"],
	7,
	TestID->"Get-pre-learning-off-clusters"
]

VerificationTest[
	SetLearningStatus[nano,False],
	Null,
	TestID->"SetLearningStatus-off"
]

VerificationTest[
	ContainsAll[Keys[RunNano[nano,Results->{ID,RI}]],{"ID","RI"}],
	True,
	TestID->"RunNano-results-no-learning"
]

VerificationTest[
	numClusts==GetNanoStatus[nano]["numClusters"],
	True,
	TestID->"test-learning-no-new-clusters"
]

VerificationTest[
	ContainsAll[Keys[GetNanoStatus[nano]],{"anomalyIndexes","clusterGrowth","clusterSizes","distanceIndexes","frequencyIndexes","numClusters","PCA","totalInferences"}],
	True,
	TestID->"GetNanoStatus-success"
]

VerificationTest[
	ContainsAll[Keys[GetNanoResults[nano]],{"DI","FI","ID","RI","SI"}],
	True,
	TestID->"GetNanoResults-success"
]

VerificationTest[
	ContainsAll[Keys[GetNanoStatus[nano,Results->{averageInferenceTime,numClusters,PCA}]],{"averageInferenceTime","numClusters","PCA"}],
	True,
	TestID->"GetNanoStatus-results-success"
]

VerificationTest[
	ContainsAll[Keys[GetNanoResults[nano,Results->{SI,ID}]], {"ID", "SI"}],
	True,
	TestID->"GetNanoResults-results-success"
]

VerificationTest[
	Dimensions[DecodePM[nano][[1]]],
	{6,8744},
	TestID->"DecodePM-success"
]

VerificationTest[
	ConfigureNano[nano,"uint16",10,ClusterMode->"streaming"],
	Null,
	TestID->"ConfigureNano-streaming-success"
]

VerificationTest[
	IntegerQ[Do[If[ContainsOnly[RunStreamingData[nano, Import[FileNameJoin[{$UserBaseDirectory,"Applications","NanoREST","ExampleData.csv"}],"CSV"]]["ID"], {0}], Null, Return[i]], {i, 1, 1000}]],
	True,
	TestID->"RunStreamingData-autotune-success"
]

VerificationTest[
	RunStreamingData[nano, Import[FileNameJoin[{$UserBaseDirectory,"Applications","NanoREST","ExampleData.csv"}],"CSV"]],
	<|"ID" -> {1, 2, 2, 2, 4, 3, 1, 3, 4, 1, 1, 1, 5, 6, 5, 5, 5, 6, 5, 1}|>,
    TestID->"RunStreamingData-cluster-success"
]

VerificationTest[
	list=#["instanceID"]&/@NanoList[nano];
	(nano["instance"]=#;
	CloseNano[nano])&/@list,
	{Null},
	TestID->"Close-all"
]