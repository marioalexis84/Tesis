BeginPackage["Paquete`", {"FeynCalc`"}]

PackageDir::usage = "Directorio de instalacion del paquete"

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

sinDenAnteojo = Get[PackageDir[] <> "../Diagramas/SinDenominador/AnteojoFinal_sinDenominador.txt"];
sinDenEarth = Get[PackageDir[] <> "../Diagramas/SinDenominador/Earth_sinDenominador.txt"];
sinDenTortuga = Get[PackageDir[] <> "../Diagramas/SinDenominador/DiagramaTortuga_sinDenominador.txt"];
sinDenT10 = Get[PackageDir[] <> "../Diagramas/SinDenominador/T10final_sinDenominador.txt"];
sinDenCilindro = Get[PackageDir[] <> "../Diagramas/SinDenominador/CilindroFinal_sinDenominador.txt"];
sinMunecoNieve = Get[PackageDir[] <> "../Diagramas/SinDenominador/MunecoNieveFinal_sinDenominador.txt"];

EndPackage[]
