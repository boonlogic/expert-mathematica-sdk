(* Wolfram Language VerificationTest file *)
BeginTestSection["UnitTests"]


VerificationTest[
	<< NanoREST`,
	Null
	,
	TestID->"Package-load"
]

VerificationTest[
	ContainsAll[Keys[nano = OpenNano["test"]],{"api-key", "api-tenant", "url", "proxy-server", "instance"}],
	True,
	TestID->"OpenNano-success"
]

VerificationTest[
	OpenNano["test", "nonexistant"],
	Null
	,
	{NanoError::handle},
	TestID->"OpenNano-user-not-found"
]

VerificationTest[
	GetVersion[nano]["api-version"],
	"/expert/v3",
	TestID->"GetVersion-success"
]

VerificationTest[
	MemberQ[#["instanceID"]&/@NanoList[nano],"test"],
	True,
  TestID->"NanoList-success"
]

VerificationTest[
	ExistsQ[nano],
	True,
	TestID->"ExistsQ-success"
]

VerificationTest[
	CloseNano[nano],
	Null,
	TestID->"CloseNano-success"
]

VerificationTest[
	ContainsAll[Keys[nano = OpenNano["test"]],{"api-key", "api-tenant", "url", "proxy-server", "instance"}],
	True,
	TestID->"OpenNano-success-1"
]

VerificationTest[
	RunNano[nano],
	Null,
	{NanoError::return},
	TestID->"RunNano-failure"
]

VerificationTest[
	LoadData[nano,Import[FileNameJoin[{$UserBaseDirectory,"Applications","NanoREST","ExampleData.csv"}],"CSV"]],
	Null,
<<<<<<< HEAD
	{NanoError::return,FileError::argerr},
=======
	{NanoError::return,FileError::argwrite},
>>>>>>> version-3-1
	TestID->"LoadData-failure-no-config"
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
	ConfigureNano[nano,ExportString[config,"JSON"]],
	Null,
	TestID->"ConfigureNano-loadconfig-success"
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
	LoadData[nano,Import[FileNameJoin[{$UserBaseDirectory,"Applications","NanoREST","ExampleData.csv"}],"CSV"]],
	Null,
	TestID->"LoadData-success"
]

VerificationTest[
	LoadData[nano,Import[FileNameJoin[{$UserBaseDirectory,"Applications","NanoREST","NoData.csv"}],"CSV"]],
	Null,
<<<<<<< HEAD
	{Import::nffil,FileError::argerr},
=======
	{Import::nffil,FileError::argwrite},
>>>>>>> version-3-1
	TestID->"LoadData-file-doesnt-exist"
]

VerificationTest[
	GetBufferStatus[nano]["totalBytesWritten"],
	400,
	TestID->"GetBufferStatus-success"
]

VerificationTest[
	ConfigureNano[nano,"int16",10,Weights->{1,2,3,4,5,6,7,8,9,10},AutotunePV->False]
	,
	Null,
	TestID->"ConfigureNano-int16-success"
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
	GetNanoResults[nano],
	Null,
	{NanoError::return},
	TestID->"GetNanoResults-failure"
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
<<<<<<< HEAD
=======
	numClusts=GetNanoStatus[nano]["numClusters"],
	7,
	TestID->"Get-pre-learning-off-clusters"
]

VerificationTest[
>>>>>>> version-3-1
	SetLearningStatus[nano,False],
	Null,
	TestID->"SetLearningStatus-off"
]

VerificationTest[
<<<<<<< HEAD
	RunNano[nano,Results->{ID,RI}],
	<|"ID" -> {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2}, "RI" -> {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 157, 0, 157, 157, 157, 157, 157, 157, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 157, 0, 157, 157, 157, 157, 157, 157}|>,
=======
	ContainsAll[Keys[RunNano[nano,Results->{ID,RI}]],{"ID","RI"}],
	True,
>>>>>>> version-3-1
	TestID->"RunNano-results-no-learning"
]

VerificationTest[
<<<<<<< HEAD
	Sort[Keys[GetNanoStatus[nano]]],
	{"anomalyIndexes","clusterGrowth","clusterSizes","distanceIndexes","frequencyIndexes","numClusters","PCA","totalInferences"},
=======
	numClusts==GetNanoStatus[nano]["numClusters"],
	True,
	TestID->"test-learning-no-new-clusters"
]

VerificationTest[
	ContainsAll[Keys[GetNanoStatus[nano]],{"anomalyIndexes","clusterGrowth","clusterSizes","distanceIndexes","frequencyIndexes","numClusters","PCA","totalInferences"}],
	True,
>>>>>>> version-3-1
	TestID->"GetNanoStatus-success"
]

VerificationTest[
	ContainsAll[Keys[GetNanoResults[nano]],{"DI","FI","ID","RI","SI"}],
	True,
	TestID->"GetNanoResults-success"
]

VerificationTest[
<<<<<<< HEAD
	Sort[Keys[GetNanoStatus[nano,Results->{averageInferenceTime,numClusters,PCA}]]],
	{"averageInferenceTime","numClusters","PCA"},
=======
	ContainsAll[Keys[GetNanoStatus[nano,Results->{averageInferenceTime,numClusters,PCA}]],{"averageInferenceTime","numClusters","PCA"}],
	True,
>>>>>>> version-3-1
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
	SaveNano[nano,"ExampleData.bn"],
	"ExampleData.bn",
	TestID->"SaveNano-success"
]

VerificationTest[
	CloseNano[nano],
	Null,
	TestID->"CloseNano-success-1"
]

VerificationTest[
	ContainsAll[Keys[OpenNano["test"]],{"api-key","api-tenant","proxy-server","url","instance"}],
	True,
	TestID->"OpenNano-success-2"
]

VerificationTest[
	GetConfig[nano],
	Null,
	{NanoError::return},
	TestID->"GetConfig-failure"
]

VerificationTest[
	CloseNano[nano],
	Null,
	TestID->"CloseNano-success-2"
]

VerificationTest[
	ContainsAll[Keys[OpenNano["test","default","Filename"->"ExampleData.bn"]],{"api-key","api-tenant","proxy-server","url","instance"}],
	True,
	TestID->"OpenNano-success-3"
]

<<<<<<< HEAD
(*VerificationTest[
	Sort[GetNanoStatus[nano]],
	<|"numClusters" -> 3, "totalInferences" -> 80, "clusterGrowth" -> {0, 1, 13}, "clusterSizes" -> {0, 52, 28}, "distanceIndexes" -> {0, 503, 486}, "frequencyIndexes" -> {0, 1203, 1014}, "anomalyIndexes" -> {1000, 0, 157}, "PCA" -> {{0, 0, 0}, {1, 0, 0}, {0, 1, 0}}|>,
	TestID->"GetNanoStatus-success-2"
]*)

VerificationTest[
	Sort[Keys[GetConfig[nano]]],
	{"accuracy", "features", "numericFormat", "percentVariation", "streamingWindowSize"},
	TestID->"GetConfig-success-2"
]

=======
VerificationTest[
	ContainsAll[Keys[GetConfig[nano]],{"accuracy", "features", "numericFormat", "percentVariation", "streamingWindowSize"}],
	True,
	TestID->"GetConfig-success-2"
]

>>>>>>> version-3-1
VerificationTest[
	CloseNano[nano],
	Null,
	TestID->"CloseNano-success-3"
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

VerificationTest[
	nano=OpenNano["test"];,
	Null,
	TestID->"OpenNano-success-4"
]

VerificationTest[
<<<<<<< HEAD
	ConfigureNano[nano,"uint16",1,0,10,0.05,10],
=======
	ConfigureNano[nano,"uint16",10,ClusterMode->"streaming"],
>>>>>>> version-3-1
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
<<<<<<< HEAD
	<|"ID" -> {2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
   1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2,
    3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 
   4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5,
    6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 
   7, 8, 9, 16, 1, 2, 3, 4, 5, 6, 7, 8, 17, 16, 1, 2, 3, 4, 12, 13, 
   14, 15, 17, 16, 18, 11, 19, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6,
    14, 8, 17, 16, 18, 11, 19, 4, 12, 13, 14, 15, 17, 16, 18, 11, 19, 
   4, 12, 6, 14, 15, 17, 16, 18, 11, 19, 4, 12, 13, 14, 8, 17, 16, 1, 
   11, 19, 4, 12, 13, 14, 15, 17, 16, 18, 2, 19, 4, 5, 6, 7, 8, 9, 10,
    1}|>,
=======
	<|"ID" -> {1, 2, 2, 2, 4, 3, 1, 3, 4, 1, 1, 1, 5, 6, 5, 5, 5, 6, 5, 1}|>,
>>>>>>> version-3-1
    TestID->"RunStreamingData-cluster-success"
]

VerificationTest[
	CloseNano[nano],
 	Null,
 	TestID->"CloseNano-success-4"
]

EndTestSection[]