 (* Wolfram Language Test file *)

BeginTestSection["Management-success"]

VerificationTest[
	<< NanoREST`,
	Null
	,
	TestID->"Package-load-1"
]

VerificationTest[
	ContainsAll[Keys[nano = OpenNano["test"]],{"api-key", "api-tenant", "url", "proxy-server", "instance"}],
	True,
	TestID->"OpenNano-success"
]

VerificationTest[
	CloseNano[nano],
	Null,
	TestID->"CloseNano"
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
	GetVersion[nano]["api-version"],
	"/expert/v3",
	TestID->"GetVersion-success"
]

VerificationTest[
	MemberQ[list=#["instanceID"]&/@NanoList[nano],"test"],
	True,
  TestID->"NanoList-success"
]

VerificationTest[
	ExistsQ[nano],
	True,
	TestID->"ExistsQ-success"
]

VerificationTest[
	ContainsAll[Keys[OpenNano["test","sample","AuthenticationPath"->path]],{"api-key", "api-tenant", "url", "proxy-server", "instance"}],
	True,
	TestID->"OpenNano-success-1"
]

VerificationTest[
	SaveNano[nano,"ExampleData.bn"],
	"ExampleData.bn",
	TestID->"SaveNano-success"
]

VerificationTest[
	SaveNano[nano,FileNameJoin[{$UserBaseDirectory,"Applications","NanoREST","ExampleData.bn"}]],
	"ExampleData.bn",
	TestID->"SaveNano-directory"
]

VerificationTest[
	StringContainsQ[SaveNano[nano],"Untitled-"],
	True,
	TestID->"SaveNano-untitled-success"
]

VerificationTest[
	StringContainsQ[SaveNano[nano],"Untitled-"],
	True,
	TestID->"SaveNano-untitled-2-success"
]

VerificationTest[
	CloseNano[nano],
	Null,
	TestID->"CloseNano-success-1"
]

VerificationTest[
	ContainsAll[Keys[OpenNano["test","sample","AuthenticationPath"->path]],{"api-key","api-tenant","proxy-server","url","instance"}],
	True,
	TestID->"OpenNano-success-2"
]

VerificationTest[
	CloseNano[nano],
	Null,
	TestID->"CloseNano-success-2"
]

VerificationTest[
	ContainsAll[Keys[OpenNano["test","default","Filename"->"ExampleData.bn","AuthenticationPath"->path]],{"api-key","api-tenant","proxy-server","url","instance"}],
	True,
	TestID->"LoadNano-success"
]

VerificationTest[
	ContainsAll[Keys[OpenNano["test","default","Filename"->FileNameJoin[{$UserBaseDirectory,"Applications","NanoREST","ExampleData.bn"}],"AuthenticationPath"->path]],{"api-key","api-tenant","proxy-server","url","instance"}],
	True,
	TestID->"LoadNano-directory-success"
]

VerificationTest[
	ContainsAll[Keys[GetConfig[nano]],{"accuracy", "features", "numericFormat", "percentVariation", "streamingWindowSize"}],
	True,
	TestID->"GetConfig-success-2"
]

VerificationTest[
	list=#["instanceID"]&/@NanoList[nano];
	(nano["instance"]=#;
	CloseNano[nano])&/@list,
	{Null},
	TestID->"Close-all-1"
]

EndTestSection[]





BeginTestSection["Management-failure"]

VerificationTest[
	<< NanoREST`,
	Null
	,
	TestID->"Package-load-2"
]

VerificationTest[
	OpenNano["test", "nonexistant"],
	Null
	,
	{NanoError::handle},
	TestID->"OpenNano-user-not-found"
]	

VerificationTest[
	path=FileNameJoin[{$UserBaseDirectory,"Applications","NanoREST",".BoonLogic.license"}];
	nano=OpenNano["test-1","sample","AuthenticationPath"->path];
	OpenNano["test-2","sample","AuthenticationPath"->path];
	OpenNano["test-3","sample","AuthenticationPath"->path];
	OpenNano["test-4","sample","AuthenticationPath"->path];
	OpenNano["test-extra","sample","AuthenticationPath"->path];,
	Null,
	{NanoError::return},
	TestID->"Open-5-instances"
]

VerificationTest[
	list=#["instanceID"]&/@NanoList[nano];
	Length[(nano["instance"]=#;
	CloseNano[nano])&/@list],
	4,
	TestID->"Close-all-2"
]

VerificationTest[
	nano["instance"]="nonexistant";
	ExistsQ[nano],
  	False,
  	TestID->"ExistsQ-failure"
]

VerificationTest[
	CloseNano[nano],
 	Null,
 	{NanoError::return},
 	TestID->"CloseNano-failure"
]

EndTestSection[]



BeginTestSection["Configuration-success"]

VerificationTest[
	<< NanoREST`,
	Null
	,
	TestID->"Package-load-3"
]

VerificationTest[
	path=FileNameJoin[{$UserBaseDirectory,"Applications","NanoREST",".BoonLogic.license"}];
	ContainsAll[Keys[nano = OpenNano["test","sample","AuthenticationPath"->path]],{"api-key", "api-tenant", "url", "proxy-server", "instance"}],
	True,
	TestID->"OpenNano-success-config"
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
	ConfigureNano[nano,"float32",10,Labels->ConstantArray["north",10],AutotuneRange->False]
	,
	Null,
	TestID->"ConfigureNano-labels-success"
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
	TestID->"Close-all-3"
]

EndTestSection[]




BeginTestSection["Configuration-failure"]

VerificationTest[
	<< NanoREST`,
	Null
	,
	TestID->"Package-load-4"
]

VerificationTest[
	path=FileNameJoin[{$UserBaseDirectory,"Applications","NanoREST",".BoonLogic.license"}];
	ContainsAll[Keys[nano = OpenNano["test","sample","AuthenticationPath"->path]],{"api-key", "api-tenant", "url", "proxy-server", "instance"}],
	True,
	TestID->"OpenNano-success-config-fail"
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
	ConfigureNano[nano,"uint16",10,Weights->{1,2,3}]
	,
	Null,
	{NanoError::length},
	TestID->"ConfigureNano-weights-length-error"
]

VerificationTest[
	ConfigureNano[nano,"uint16",10,Labels->{"l1","l2","l3"}]
	,
	Null,
	{NanoError::length},
	TestID->"ConfigureNano-labels-length-error"
]

VerificationTest[
	ConfigureNano[nano,"uint16",10,AutotuneByFeature->All]
	,
	Null,
	{InvalidOption::option},
	TestID->"ConfigureNano-byfeature-error"
]

VerificationTest[
	ConfigureNano[nano,"uint16",10,AutotunePV->All]
	,
	Null,
	{InvalidOption::option},
	TestID->"ConfigureNano-autotunepv-error"
]

VerificationTest[
	ConfigureNano[nano,"uint16",10,AutotuneRange->All]
	,
	Null,
	{InvalidOption::option},
	TestID->"ConfigureNano-autotunerange-error"
]

VerificationTest[
	ConfigureNano[nano,"uint16",10,StreamAutotune->All]
	,
	Null,
	{InvalidOption::option},
	TestID->"ConfigureNano-streamautotune-error"
]

VerificationTest[
	ConfigureNano[nano,"uint16",10,StreamGraduation->All]
	,
	Null,
	{InvalidOption::option},
	TestID->"ConfigureNano-streamgrad-error"
]

VerificationTest[
	list=#["instanceID"]&/@NanoList[nano];
	(nano["instance"]=#;
	CloseNano[nano])&/@list,
	{Null},
	TestID->"Close-all-4"
]

EndTestSection[]




BeginTestSection["Data-creation"]

VerificationTest[
	Dimensions[native=GenerateRandomPatternNative[100,10]],
	{100},
	TestID->"Native-template"
]

VerificationTest[
	Dimensions[GenerateRandomPatternNative[{10,11,12,13,14,15,100,101,102,103}]],
	{10},
	TestID->"Native-template-by-feature"
]

VerificationTest[
	GenerateRandomPatternNative[{{1,2,3},{2,3,4}}],
	Null,
	{InvalidParam::argerr},
	TestID->"Native-invlid-max-array"
]

VerificationTest[
	Dimensions[int=GenerateRandomPatternInt[100,0,10]],
	{100},
	TestID->"Int-template"
]

VerificationTest[
	Dimensions[GenerateRandomPatternInt[{0,1,2,3,4,5,10,11,12,13},{10,11,12,13,14,15,100,101,102,103}]],
	{10},
	TestID->"Int-template-by-feature"
]

VerificationTest[
	GenerateRandomPatternInt[{{1,2,3},{2,3,4}},{{11,12,13},{12,13,14}}],
	Null,
	{NanoError::length},
	TestID->"Int-invlid-max-array"
]

VerificationTest[
	Dimensions[float=GenerateRandomPatternFloat[100,0,10]],
	{100},
	TestID->"Float-template"
]

VerificationTest[
	Dimensions[GenerateRandomPatternFloat[{0,1,2,3,4,5,10,11,12,13},{10,11,12,13,14,15,100,101,102,103}]],
	{10},
	TestID->"Float-template-by-feature"
]

VerificationTest[
	GenerateRandomPatternFloat[{{1,2,3},{2,3,4}},{{11,12,13},{12,13,14}}],
	Null,
	{NanoError::length},
	TestID->"Float-invlid-max-array"
]

VerificationTest[
	Dimensions[native=GenerateRandomVariantNative[native,10,0.1,10]],
	{10,100},
	TestID->"Native-variant-cloud"
]

VerificationTest[
	GenerateRandomVariantFloat[float,0,10,0.1,10,RandomInteger[{1,10},10]],
	Null,
	{NanoError::length},
	TestID->"Float-variant-weight-error"
]

VerificationTest[
	Dimensions[int=GenerateRandomVariantInt[int,0,10,0.1,10,RandomInteger[{1,10},100]]],
	{10,100},
	TestID->"Int-variant-weight-cloud"
]

VerificationTest[
	Dimensions[float=GenerateRandomVariantFloat[float,0,10,0.1,10,1,Exact->False]],
	{10,100},
	TestID->"Float-variant-weight-cloud"
]

VerificationTest[
	GenerateRandomVariantFloat[int,0,10,0.1,10,1,Exact->All],
	Null,
	{NanoError::option},
	TestID->"Float-option-invlid"
]

VerificationTest[
	Length[Select[ComputePercentVariation[native[[1]],#,0,10]&/@native,#>.1&]]<2,
	True,
	TestID->"PV-test"
]
	
VerificationTest[
	ComputePercentVariation[native[[1]],{1,2,3},0,10],
	Null,
	{NanoError::length},
	TestID->"Pattern-length-mismatch"
]

VerificationTest[
	ComputePercentVariation[native[[1]],native[[2]],0,10,{1,2,3}],
	Null,
	{NanoError::length},
	TestID->"Weights-length-error"
]

VerificationTest[
	ComputePercentVariation[native[[1]],native[[2]],{0,1,2},10,{1,2,3}],
	Null,
	{NanoError::length},
	TestID->"Min-Max-length-error"
]

EndTestSection[]





BeginTestSection["Cluster-success"]

VerificationTest[
	<< NanoREST`,
	Null
	,
	TestID->"Package-load-5"
]

VerificationTest[
	path=FileNameJoin[{$UserBaseDirectory,"Applications","NanoREST",".BoonLogic.license"}];
	ContainsAll[Keys[nano = OpenNano["test", "sample", 
								"AuthenticationPath"->path]
					],{"api-key", "api-tenant", "url", "proxy-server", "instance"}],
	True,
	TestID->"OpenNano-cluster"
]

VerificationTest[
	ConfigureNano[nano,"uint16",10,MinVals->0,MaxVals->10,PercentVariation->0.07,Weights->1,StreamingWindow->1,NanoAccuracy->0.99]
	,
	Null,
	TestID->"ConfigureNano-cluster"
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
	TestID->"ConfigureNano-float32-cluster"
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
	TestID->"ConfigureNano-autotune-overall-cluster"
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
	TestID->"ConfigureNano-autotune-excludes-cluster"
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
	TestID->"ConfigureNano-autotune-cluster"
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
	Length[DecodePM[nano,Results->{"CMYK","SourceVector","BinaryPM","Features"},RowSort->True]],
	4,
	TestID->"DecodePM-all-success"
]

VerificationTest[
	ConfigureNano[nano,"uint16",10,ClusterMode->"streaming"],
	Null,
	TestID->"ConfigureNano-streaming-cluster"
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
	TestID->"Close-all-5"
]

EndTestSection[]




BeginTestSection["Cluster-failure"]

VerificationTest[
	<< NanoREST`,
	Null
	,
	TestID->"Package-load-6"
]

VerificationTest[
	path=FileNameJoin[{$UserBaseDirectory,"Applications","NanoREST",".BoonLogic.license"}];
	ContainsAll[Keys[nano = OpenNano["VerificationTest", "sample", 
								"AuthenticationPath"->path]
					],{"api-key", "api-tenant", "url", "proxy-server", "instance"}],
	True,
	TestID->"OpenNano-cluster-fail"
]

VerificationTest[
	RunNano[nano],
	Null,
	{NanoError::return},
	TestID->"RunNano-failure"
]

VerificationTest[
	LoadData[nano,data=Import[FileNameJoin[{$UserBaseDirectory,"Applications","NanoREST","ExampleData.csv"}],"CSV"]],
	Null,
	{NanoError::return,FileError::argwrite},
	TestID->"LoadData-failure-no-config"
]

VerificationTest[
	RunStreamingData[nano,data],
	Null,
	{NanoError::return,FileError::argwrite},
	TestID->"StreamingData-failure-no-config"
]

VerificationTest[
	LoadData[nano,data,AppendData->All],
	Null,
	{InvalidOption::option},
	TestID->"LoadData-appenddata-error"
]

VerificationTest[
	LoadData[nano,data,GZip->All],
	Null,
	{InvalidOption::option},
	TestID->"LoadData-gzip-error"
]

VerificationTest[
	RunStreamingData[nano,data,GZip->All],
	Null,
	{InvalidOption::option},
	TestID->"Streaming-gzip-error"
]

VerificationTest[
	RunStreamingData[nano,data,Results->"Bogus"],
	Null,
	{InvalidOption::option},
	TestID->"Streaming-results-error"
]

VerificationTest[
	RunNano[nano,Results->"Bogus"],
	Null,
	{InvalidOption::option},
	TestID->"RunNano-results-error"
]

VerificationTest[
	DecodePM[nano,Results->"Bogus"],
	Null,
	{InvalidOption::option},
	TestID->"DecodePM-results-error"
]

VerificationTest[
	DecodePM[nano,RowSort->"Bogus"],
	Null,
	{InvalidOption::option},
	TestID->"DecodePM-rowsort-error"
]

VerificationTest[
	GetNanoStatus[nano,Results->"Bogus"],
	Null,
	{InvalidOption::option},
	TestID->"Status-results-error"
]

VerificationTest[
	GetNanoResults[nano,Results->"Bogus"],
	Null,
	{InvalidOption::option},
	TestID->"Results-results-error"
]

VerificationTest[
	GetNanoStatus[nano],
	Null,
	{NanoError::return},
	TestID->"Status-no-results"
]

VerificationTest[
	ConfigureNano[nano,"uint16",10,MinVals->0,MaxVals->10,PercentVariation->0.07,Weights->1,StreamingWindow->1,NanoAccuracy->0.99]
	,
	Null,
	TestID->"ConfigureNano-uint16-cluster-fail"
]

VerificationTest[
	GetNanoResults[nano],
	Null,
	{NanoError::return},
	TestID->"Results-no-results"
]

VerificationTest[
	AutotuneConfig[nano],
	Null,
	{NanoError::return},
	TestID->"AutotuneConfig-failure-no-data"
]

VerificationTest[
	LoadData[nano,Import[FileNameJoin[{$UserBaseDirectory,"Applications","NanoREST","NoData.csv"}],"CSV"]],
	Null,
	{Import::nffil,FileError::argwrite},
	TestID->"LoadData-file-doesnt-exist"
]

VerificationTest[
	LoadData[nano,Import[FileNameJoin[{$UserBaseDirectory,"Applications","NanoREST","ExampleData.csv"}],"CSV"]],
	Null,
	TestID->"LoadData-cluster-fail"
]

VerificationTest[
	GetNanoResults[nano],
	Null,
	{NanoError::return},
	TestID->"GetNanoResults-failure"
]

VerificationTest[
	list=#["instanceID"]&/@NanoList[nano];
	(nano["instance"]=#;
	CloseNano[nano])&/@list,
	{Null},
	TestID->"Close-all-6"
]

VerificationTest[
	LoadNano[nano,"ExampleData.bn"],
	Null,
	{NanoError::return},
	TestID->"LoadNano-no-instance"
]

EndTestSection[]

