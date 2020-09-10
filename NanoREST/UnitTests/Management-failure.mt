(* Wolfram Language Test file *)

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
	TestID->"Close-all"
]

VerificationVerificationTest[
	nano["instance"]="nonexistant";
	ExistsQ[nano],
  	False,
  	TestID->"ExistsQ-failure"
]

VerificationVerificationTest[
	CloseNano[nano],
 	Null,
 	{NanoError::return},
 	TestID->"CloseNano-failure"
]