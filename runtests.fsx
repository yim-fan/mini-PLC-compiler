
#r "bin/FsLexYacc.Runtime.dll";;
#load "/Users/yim/Desktop/Project/Environ.fs";;
#load "/Users/yim/Desktop/Project/Absyn.fs";;
#load "/Users/yim/Desktop/Project/PlcParserAux.fs";;
#load "/Users/yim/Desktop/Project/PlcParser.fs";;
#load "/Users/yim/Desktop/Project/PlcLexer.fs";;
#load "/Users/yim/Desktop/Project/Parse.fs";;
#load "/Users/yim/Desktop/Project/TestAux.fs";;
#load "/Users/yim/Desktop/Project/PlcInterp.fs";;
#load "/Users/yim/Desktop/Project/PlcChecker.fs";;
#load "/Users/yim/Desktop/Project/Plc.fs";;

open Absyn
open TestAux
let fromString = Parse.fromString // string parser function
let run e = printfn "\nResult is  %s\n" (Plc.run e)   // execution function
#load "/Users/yim/Desktop/Project/Test.fs";

testAll Test.cases
