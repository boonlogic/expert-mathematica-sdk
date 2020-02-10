# Mathematica SDK Documentation
Welcome to the documentation for the Boon Nano Mathematica package!

## Getting Started
Mathematica documentation is located within Mathematica itself. See below for instructions on setting up authentication and loading the package.

---------
### Setup
1. Download and save the `.Boonlogic` file in user home directory (ie Mac: /Users/'username' or Windows: C:\\Users\\'username'). This file can be found in the welcome email for the BoonNano NanoSaas.
>NOTE: This file is what the SDK's look for to access the API. If it is placed somewhere other than the home directory, when opening a new nano, the file path will have to be specified.

2. Clone this repository
2. Open a new Mathematica notebook
3. Set the directory to the cloned repo's location
```
SetDirectory["repo/location"]
```
4. Run the `UpdateNano` package
```
<<UpdateNano`
```
5. Load the package
```
<<NanoREST`
```

>__NOTE: `UpdateNano` only needs to be run the first time or whenever a new version of the package is available.   __

-------
### Access Documentation
1. Open Mathematica
2. In the menu, go to Help -> Wolfram Documentation
3. Search NanoREST to get the guide page

or   
1. Open Mathematica
2. Type the function you want more information on
3. Highlight the function name
4. In the menu, go to Help -> Find Selected Function

>*NOTE: If the kernel is already running and the Mathematica help documentations have already been accessed, the package's new documentation will not be loaded. In this case, quit the kernel and open up a NanoREST documentation page first*
