(* Wolfram Language Test file *)

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
	TestID->"OpenNano-success-3"
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
	TestID->"Close-all"
]
