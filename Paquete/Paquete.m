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
Get[PackageDir[] <> "Analisis/colector.m"]

denD = 1/(D-2)^2;
sinDenAnteojo = Get[PackageDir[] <> "../Diagramas/SinDenominador/AnteojoFinal_sinDenominador.txt"];
sinDenEarth = Get[PackageDir[] <> "../Diagramas/SinDenominador/Earth_sinDenominador.txt"];
sinDenT10 = Get[PackageDir[] <> "../Diagramas/SinDenominador/T10final_sinDenominador.txt"];
sinDenT10ghost2 = Get[PackageDir[] <> "../Diagramas/SinDenominador/T10ghost2Final_sinDenominador.txt"];
sinDenT10ghost1 = Get[PackageDir[] <> "../Diagramas/SinDenominador/T10ghost1_sinDenominador.txt"];
sinDenCilindro = Get[PackageDir[] <> "../Diagramas/SinDenominador/CilindroFinal_sinDenominador.txt"];
sinDenMunecoNieve = Get[PackageDir[] <> "../Diagramas/SinDenominador/MunecoNieveFinal_sinDenominador.txt"];
sinDenMunecoNieveGhost = Get[PackageDir[] <> "../Diagramas/SinDenominador/MunecoNieveGhost_sinDenominador.txt"];
sinDenCil1LoopGhost = Get[PackageDir[] <> "../Diagramas/SinDenominador/Cilindro1LoopGhostGoya_sinDenominador.txt"];
sinDenCil2LoopGhost = Get[PackageDir[] <> "../Diagramas/SinDenominador/Cilindro2LoopGhostGoya_sinDenominador.txt"];
sinDenCilCirc = Get[PackageDir[] <> "../Diagramas/SinDenominador/CilindroGhostCirculanteGoya_sinDenominador.txt"];
sinDenTortuga = Get[PackageDir[] <> "../Diagramas/SinDenominador/Tortuga_sinDenominador.txt"];
sinDenOso = Get[PackageDir[] <> "../Diagramas/SinDenominador/Oso_sinDenominador.txt"];

EndPackage[]
