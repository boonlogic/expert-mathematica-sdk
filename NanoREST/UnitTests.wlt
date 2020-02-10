(* Wolfram Language VerificationTest file *)
BeginTestSection["UnitTests"]


VerificationTest[
	<< NanoREST`,
	Null
	,
	TestID->"Package-load"
]

VerificationTest[
	nano = OpenNano["test"],
	<|"api-key" -> "05b44fa3aa8a46c7bcd27c0def74915ea663dc4f0f836a43", 
 "api-tenant" -> "b97b89a6f84071c1", 
 "url" -> "https://b97b89a6f84071c1.boonlogic.com/expert/v3/", 
 "instance" -> "test"|>,
	TestID->"OpenNano-success-1"
]

VerificationTest[
	OpenNano["test", "nonexistant"],
	Null
	,
	{NanoError::handle},
	TestID->"OpenNano-user-not-found"
]

VerificationTest[
	GetVersion[nano],
	<|"api-version" -> "/expert/v3", "boon-nano" -> "eef0f182", "expert-api" -> "d849b2f7", "expert-common" -> "829a7477"|>,
	TestID->"GetVersion-success"
]

VerificationTest[
	#["instanceID"]&/@NanoList[nano],
	{"test"},
  TestID->"NanoList-success"
]

VerificationTest[
	ExistsQ[nano],
	True,
	TestID->"ExistsQ-success"
]

VerificationTest[
	ExistsQ[<|"api-key" -> 
   "05b44fa3aa8a46c7bcd27c0def74915ea663dc4f0f836a43", 
  "api-tenant" -> "b97b89a6f84071c1", 
  "url" -> "https://b97b89a6f84071c1.boonlogic.com/expert/v3/", 
  "instance" -> "nonexistant"|>],
  False,
  TestID->"ExistsQ-failure"
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
	{NanoError::return,Export::infer,General::erropts,NanoError::return},
	TestID->"LoadData-failure-no-config"
]

VerificationTest[
	ConfigureNano[nano,"uint16",10,0,10,0.07,1,1,0.99]
	,
	Null,
	TestID->"ConfigureNano-uint16-success"
]

VerificationTest[
	config=GetConfig[nano],
	<|"accuracy" -> 0.99, "features" -> {<|"maxVal" -> 10, "minVal" -> 0, "weight" -> 1|>, <|"maxVal" -> 10, "minVal" -> 0, "weight" -> 1|>, <|"maxVal" -> 10, "minVal" -> 0, "weight" -> 1|>, <|"maxVal" -> 10, "minVal" -> 0, "weight" -> 1|>, <|"maxVal" -> 10, "minVal" -> 0, "weight" -> 1|>, <|"maxVal" -> 10, "minVal" -> 0, "weight" -> 1|>, <|"maxVal" -> 10, "minVal" -> 0, "weight" -> 1|>, <|"maxVal" -> 10, "minVal" -> 0, "weight" -> 1|>, <|"maxVal" -> 10, "minVal" -> 0, "weight" -> 1|>, <|"maxVal" -> 10, "minVal" -> 0, "weight" -> 1|>}, "numericFormat" -> "uint16", "percentVariation" -> 0.07, "streamingWindowSize" -> 1|>,
	TestID->"GetConfig-success-1"
]

VerificationTest[
	ConfigureNano[nano,"float32",10,ConstantArray[0,10],10,0.07,1,1,0.99]
	,
	Null,
	TestID->"ConfigureNano-float32-success"
]

VerificationTest[
	ConfigureNano[nano,"int16",10,0,10,0.07,1,{1,2,3,4,5,6,7,8,9,10},0.99]
	,
	Null,
	TestID->"ConfigureNano-int16-success"
]

VerificationTest[
	ConfigureNano[nano,"uint16",10,0,ConstantArray[10,9],0.07,1,1,0.99]
	,
	Null,
	{NanoError::length},
	TestID->"ConfigureNano-length-failure"
]

VerificationTest[
	ConfigureNano[nano,"uint16",10,1,10,0.07,1,1,0.99]
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
	{Import::nffil,BinaryWrite::nocoerce,General::erropts,NanoError::return},
	TestID->"LoadData-file-doesnt-exist"
]

VerificationTest[
	GetBufferStatus[nano],
	<|"totalBytesInBuffer" -> 400, "totalBytesProcessed" -> 0, "totalBytesWritten" -> 400|>,
	TestID->"GetBufferStatus-success"
]

VerificationTest[
	AutotuneConfig[nano,AutotunePV->False],
	Null,
	TestID->"AutotuneConfig-just-range-success"
]

VerificationTest[
	AutotuneConfig[nano,AutotuneRange->False],
	Null,
	TestID->"AutotuneConfig-just-pv-success"
]

VerificationTest[
	AutotuneConfig[nano,AutotunePV->False,ByFeature->True],
	Null,
	TestID->"AutotuneConfig-byfeature-success"
]

VerificationTest[
	AutotuneConfig[nano,AutotunePV->False,ByFeature->True,Excludes->{1,10}],
	Null,
	TestID->"AutotuneConfig-byfeature-exclusions-success"
]

VerificationTest[
	AutotuneConfig[nano],
	Null,
	TestID->"AutotuneConfig-all-success"
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
	GetNanoStatus[nano],
	Null,
	{NanoError::return},
	TestID->"GetNanoStatus-failure"
]

VerificationTest[
	GetBufferStatus[nano],
	<|"totalBytesInBuffer" -> 800, "totalBytesProcessed" -> 0, "totalBytesWritten" -> 800|>,
	TestID->"GetBufferStatus-success-1"
]

VerificationTest[
	RunNano[nano],
	Null,
	TestID->"RunNano-success"
]

VerificationTest[
	RunNano[nano,Results->{ID,RI}],
	<|"ID" -> {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2}, "RI" -> {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 157, 0, 157, 157, 157, 157, 157, 157, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 157, 0, 157, 157, 157, 157, 157, 157}|>,
	TestID->"RunNano-results-success"
]

VerificationTest[
	GetNanoStatus[nano],
	<|"PCA" -> {{0, 0, 0}, {1, 0, 0}, {0, 1, 0}}, "anomalyIndexes" -> {1000, 0, 157}, "clusterGrowth" -> {0, 1, 13}, "clusterSizes" -> {0, 52, 28}, "distanceIndexes" -> {0, 503, 486}, "frequencyIndexes" -> {0, 1203, 1014}, "numClusters" -> 3, "totalInferences" -> 80|>,
	TestID->"GetNanoStatus-success"
]

VerificationTest[
	GetNanoResults[nano],
	<|"DI" -> {503, 503, 503, 503, 503, 503, 503, 503, 503, 503, 503, 503, 486, 503, 486, 486, 486, 486, 486, 486, 503, 503, 503, 503, 503, 503, 503, 503, 503, 503, 503, 503, 486, 503, 486, 486, 486, 486, 486, 486}, "FI" -> {1203, 1203, 1203, 1203, 1203, 1203, 1203, 1203, 1203, 1203, 1203, 1203, 1014, 1203, 1014, 1014, 1014, 1014, 1014, 1014, 1203, 1203, 1203, 1203, 1203, 1203, 1203, 1203, 1203, 1203, 1203, 1203, 1014, 1203, 1014, 1014, 1014, 1014, 1014, 1014}, "ID" -> {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2}, "RI" -> {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 157, 0, 157, 157, 157, 157, 157, 157, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 157, 0, 157, 157, 157, 157, 157, 157}, "SI" -> {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 3, 7, 14, 29, 58, 117, 77, 154, 152, 147, 137, 117, 78, 0, 0, 0, 0, 0, 0, 1, 3, 7, 14, 29, 58}|>,
	TestID->"GetNanoResults-success"
]

VerificationTest[
	Keys[GetNanoStatus[nano,Results->{averageInferenceTime,numClusters,PCA}]],
	{"PCA","averageInferenceTime","numClusters"},
	TestID->"GetNanoStatus-results-success"
]

VerificationTest[
	GetNanoResults[nano,Results->{SI,ID}],
	<|"ID" -> {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2},"SI" -> {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 3, 7, 14, 29, 58, 117, 77, 154, 152, 147, 137, 117, 78, 0, 0, 0, 0, 0, 0, 1, 3, 7, 14, 29, 58}|>,
	TestID->"GetNanoResults-results-success"
]

VerificationTest[
	Dimensions[DecodePM[nano][[1]]],
	{2,8320},
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
	OpenNano["test"],
	<|"api-key" -> "05b44fa3aa8a46c7bcd27c0def74915ea663dc4f0f836a43", 
 	"api-tenant" -> "b97b89a6f84071c1", 
 	"url" -> "https://b97b89a6f84071c1.boonlogic.com/expert/v3/", 
 	"instance" -> "test"|>,
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
	OpenNano["test","default","Filename"->"ExampleData.bn"],
	<|"api-key" -> "05b44fa3aa8a46c7bcd27c0def74915ea663dc4f0f836a43", 
 	"api-tenant" -> "b97b89a6f84071c1", 
 	"url" -> "https://b97b89a6f84071c1.boonlogic.com/expert/v3/", 
 	"instance" -> "test"|>,
	TestID->"OpenNano-success-3"
]

VerificationTest[
	GetNanoStatus[nano],
	<|"PCA" -> {{0, 0, 0}, {1, 0, 0}, {0, 1, 0}}, "anomalyIndexes" -> {1000, 0, 157}, "clusterGrowth" -> {0, 1, 13}, "clusterSizes" -> {0, 52, 28}, "distanceIndexes" -> {0, 503, 486}, "frequencyIndexes" -> {0, 1203, 1014}, "numClusters" -> 3, "totalInferences" -> 80|>,
	TestID->"GetNanoStatus-success-2"
]


VerificationTest[
	CloseNano[<|"api-key" -> "05b44fa3aa8a46c7bcd27c0def74915ea663dc4f0f836a43", 
 	"api-tenant" -> "b97b89a6f84071c1", 
 	"url" -> "https://b97b89a6f84071c1.boonlogic.com/expert/v3/", 
 	"instance" -> "nonexistant"|>],
 	Null,
 	{NanoError::return},
 	TestID->"CloseNano-failure"
]

VerificationTest[
	CloseNano[nano],
	Null,
	TestID->"CloseNano-success-3"
]

EndTestSection[]