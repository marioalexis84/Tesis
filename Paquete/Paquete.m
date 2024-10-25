BeginPackage["Paquete`", {"FeynCalc`"}]

PackageDir::usage = "Directorio de instalacion del paquete"

Analisis::usage = "Analisis";
den::usage ="aa"

Begin["`Private`"]

$PackageDir = DirectoryName[$InputFileName];
PackageDir[] := $PackageDir;
(*
Paquete`Analisis = Get[PackageDir[] <> "Analisis/analisisSinDenominador.m"]
*)

End[]

Print[PackageDir[]]
Get[PackageDir[] <> "Analisis/analisisSinDenominador.m"]
Get[PackageDir[] <> "Analisis/denominadores.m"]

EndPackage[]
