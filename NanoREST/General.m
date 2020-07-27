(* Wolfram Language Package *)

BeginPackage["NanoREST`"]
Unprotect["NanoREST`*"]
ClearAll["NanoREST`*"]
ClearAll["NanoREST`Protected`*"]

(*Error Messages*)
NanoError::length="Lengths of unequal value"
NanoError::return="Failed with an error code of `1` and body: `2`"
InvalidParam::argerr="The first parameter must either be a list of magnitudes or a pattern length"
FileError::argerr="File not found"
FileError::argwrite="File write error"
NanoError::handle="`1` is not a valid nano reference"
InvalidOption::option="`1` is not a valid option value for `2`"
MissingParameter::argerr="`1` was not provided or was invalid"
NanoWarning::message="`1`"

GetVersion::usage="GetVersion[] returns the nano version running"

(* OTHER *)
GenerateRandomPatternNative::usage="GenerateRandomPatternNative[PatternLength,MaxVal] generates a random pattern of integers with values from 0 to max with length PatternLength
\nGenerateRandomPatternNative[MaxVal] generates a random pattern of integers with values from 0 to the corresponding max in each column"

GenerateRandomPatternInt::usage="GenerateRandomPatternInt[PatternLength,MinVal,MaxVal] generates a random pattern of integers with values from min to max with length PatternLength
\nGenerateRandomPatternInt[MinVal,MaxVal] generates a random pattern of integers with values from the corresponding mins to maxes in each column"

GenerateRandomPatternFloat::usage="GenerateRandomPatternFloat[PatternLength,MinVal,MaxVal] generates a random pattern of floats with values from min to max with length PatternLength
\nGenerateRandomPatternFloat[MinVal,MaxVal] generates a random pattern of floats with values from the corresponding mins and maxes in each column"

ComputePercentVariation::usage="ComputePercentVariation[v1,v2,MinVal,MaxVal] calculates the percent variation between v1 and v2 using the given parameters
\nComputePercentVariation[v1,v2,MinVal,MaxVal,WeightedVals] calculates the percent variation between v1 and v2 using the given parameters and weights"

GenerateRandomVariantInt::usage="GenerateRandomVariantInt[SourcePattern,MinVal,MaxVal,PercentVariation] generates a pseudo random integer vector variant with values from min to max
\nGenerateRandomVariantInt[SourcePattern,MinVal,MaxVal,PercentVariation,Num] generates Num pseudo random integer vector variant(s) with values from min to max"

GenerateRandomVariantNative::usage="GenerateRandomVariantNative[SourcePattern,MaxVal,PercentVariation] generates a pseudo random integer vector variant with values from 0 to max
\nGenerateRandomVariantNative[SourcePattern,MaxVal,PercentVariation,Num] generates Num pseudo random integer vector variant(s) from SourcePattern with values from 0 to max"

GenerateRandomVariantFloat::usage="GenerateRandomVariantFloat[SourcePattern,MinVal,MaxVal,PercentVariation] generates a pseudo random float vector variant with values from min to max
\nGenerateRandomVariantFloat[SourcePattern,MinVal,MaxVal,PercentVariation,Num] generates Num pseudo random float vector variant(s) with values from min to max"

Exact::usage="whether to make the variant vector exactly the given percent variation away from the source pattern"


Begin["`Private`"] (* Begin Private Context *) 

(*messageHandler = If[Last[#], Abort[]] &;
Internal`AddHandler["Message", messageHandler];*)

(* Syntax 1: GenerateRandomPatternNative[5,13] generate a random pattern of length 5 of max 13 *)
(* Syntax 2: GenerateRandomPatternNative[{17,5,30,5}] generate a random pattern of length 4 the max values given *)
GenerateRandomPatternNative[Param1_,Max_:0]:=Module[{},
	If[Depth[Param1]==1,(* this is syntax 1 *)
	Return[RandomInteger[{0,Max},Param1]]];
		
	If[Depth[Param1]!=2,Message[InvalidParam::argerr];
		Return[]
	];
	Return[RandomInteger[{0,#}]&/@Param1]
]

(* If Param1 and Param2 are lists, then those lists are used as corresponding min and max for generating the pattern*)
(* Otherwise, Param1 is the pattern length, Param2 is the min, and Param3 is the max *)
GenerateRandomPatternInt[Param1_,Param2_,Param3_:0]:=Module[{},
	If[Depth[Param1]==1,(* this is syntax 1 *)
		Return[RandomInteger[{Param2,Param3},Param1]]
	];
	If[Depth[Param1]!=2||Depth[Param1]!=2||Length[Param1]!=Length[Param2],
		Message[NanoError::length];
		Return[]
	];
	Return[RandomInteger[#]&/@Transpose[{Param1,Param2}]]
]

GenerateRandomPatternFloat[Param1_,Param2_,Param3_:0]:=Module[{},
	If[Depth[Param1]==1,(* this is syntax 1 *)
		Return[RandomReal[{Param2,Param3},Param1]]
	];
	If[Depth[Param1]!=2||Depth[Param1]!=2||Length[Param1]!=Length[Param2],
		Message[NanoError::length];
		Return[]
	];
	Return[RandomReal[#]&/@Transpose[{Param1,Param2}]]
]

(* Min and Max are either numbers or lists the same length as V1 and V2 (for variable ranges) *)
ListClip[L_,Min_, Max_]:=Table[Clip[L[[i]],{Min[[i]],Max[[i]]}],{i,1,Length[L]}]
ComputePercentVariation[V1_,V2_,Min_,Max_,WeightsIn_:1]:=Module[{PatternLength,ExpandedMinVal=Min, ExpandedMax=Max,ColPercentDifferences,ColRelativeWeights,Weights},
	If[Length[V1]!=Length[V2],
		Message[NanoError::length];
		Return[]
	];
	PatternLength=Length[V1];

	If[Depth[Min]==1, ExpandedMinVal=ConstantArray[Min,PatternLength]];
	If[Depth[Max]==1, ExpandedMax=ConstantArray[Max,PatternLength]];
	
	If[Length[ExpandedMinVal]!=Length[V2],
		Message[NanoError::length];
		Return[]
	];
	
	If[WeightsIn===1,Weights=ConstantArray[1,PatternLength],
	If[Length[WeightsIn]!=PatternLength, Message[NanoError::length];Return[]],
	Weights=WeightsIn];
	
	ColPercentDifferences=Abs[ListClip[V1,ExpandedMinVal,ExpandedMax]-ListClip[V2,ExpandedMinVal,ExpandedMax]]/(ExpandedMax-ExpandedMinVal+1);
	ColRelativeWeights=Weights/Total[Weights];
	Return[Total[N[ColPercentDifferences*ColRelativeWeights]]];
]

Options[GenerateRandomVariantFloat]={Exact->True};
(* Create a variant of SourcePattern less than or equal to the PercentVariation *)
GenerateRandomVariantFloat[SourcePattern_,Min_,Max_,PercentVariation_,Num_:1,WeightsIn_:1,opts:OptionsPattern[]]:=Module[{allVariants={},i,pv,VariationDirection,PatternLength,IndexToChange,VariantPattern,AccumulatedVariation,ExpandedMinArray = Min, ExpandedMaxArray = Max,IncrementPerFeature,Variation,VariationPerIncrement,Weights},
	If[MemberQ[{True,False},OptionValue[Exact]]==False,
		Message[NanoError::option,
		ToString[OptionValue[Exact]],ToString[Exact]];
		Return[]
	];
	
	PatternLength=Length[SourcePattern];
	If[Depth[Min]==1, ExpandedMinArray=ConstantArray[Min,PatternLength]];
	If[Depth[Max]==1, ExpandedMaxArray=ConstantArray[Max,PatternLength]];
	
	If[WeightsIn===1,
		Weights=ConstantArray[1,PatternLength],
		If[Length[WeightsIn]!=PatternLength,
			Message[NanoError::length];
			Return[]
		];
		Weights=WeightsIn
	];

	For[i=1,i<=Num,i++,
		VariantPattern=SourcePattern;
		VariationDirection=RandomChoice[{-1,1},PatternLength];
		AccumulatedVariation=0;
		pv=If[OptionValue[Exact]==True,PercentVariation,RandomReal[{0.0001,PercentVariation}]];(*Depending if you want the cloud affect or exactly the percent variation*)
		IncrementPerFeature=(ExpandedMaxArray-ExpandedMinArray+1)/1000.0; (*Increment by 100ths of a percent in each feature *)
		VariationPerIncrement=(Weights/Total[Weights])/1000.0;
		While[AccumulatedVariation<pv,
			IndexToChange=RandomInteger[{1,PatternLength}];
			Variation=VariationDirection[[IndexToChange]]*IncrementPerFeature[[IndexToChange]];
			If[ExpandedMinArray[[IndexToChange]]<= VariantPattern[[IndexToChange]]+Variation<=ExpandedMaxArray[[IndexToChange]],
				VariantPattern[[IndexToChange]]+=Variation;
				AccumulatedVariation+=VariationPerIncrement[[IndexToChange]];
			]
		];
		AppendTo[allVariants,VariantPattern];
	];
	Return[allVariants];
]

Options[GenerateRandomVariantNative]={Exact->True};
Options[GenerateRandomVariantInt]={Exact->True};
GenerateRandomVariantNative[SourcePattern_,Max_,PercentVariation_,Num_:1,Weights_:1,OptionsPattern[]]:=Round[GenerateRandomVariantFloat[SourcePattern,0,Max,PercentVariation,Num,Weights,Exact->OptionValue[Exact]]];
GenerateRandomVariantInt[SourcePattern_,Min_,Max_,PercentVariation_,Num_:1,Weights_:1,OptionsPattern[]]:=Round[GenerateRandomVariantFloat[SourcePattern,Min,Max,PercentVariation,Num,Weights,Exact->OptionValue[Exact]]]

(* Get server version information *)
GetVersion[NanoHandle_]:=Module[{req,RetVal},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	req = HTTPRequest[StringDrop[NanoHandle["url"],-3]<>"version"<>"?api-tenant="<>NanoHandle["api-tenant"], 
	<|
	"Method" ->"GET",
	"Headers"->{"Content-Type"->"application/json","x-token"->NanoHandle["api-key"]}|>];
	RetVal= URLRead[req,{"Status","Body"}];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];
		Return[];
	];
	Return[ImportString[RetVal[[2]],"RawJSON"]];
]

End[] (* End Private Context *)

Protect[GetVersion]

Protect[GenerateRandomPatternNative]
Protect[GenerateRandomPatternInt]
Protect[GenerateRandomPatternFloat]
Protect[ComputePercentVariation]
Protect[GenerateRandomVariantInt]
Protect[GenerateRandomVariantNative]
Protect[GenerateRandomVariantFloat]

EndPackage[]