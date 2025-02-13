(*
tablaT10ghost1[] := Module[
    {paso, filtrado, terminos},

    paso = Filtros[sinDenT10ghost1 /. {k1 -> -k1}, denT10ghost1 /. {k1 -> -k1}, D, 7];

    filtrado = filtrarExpandSeq[
        paso,
        {{k12 -> 1/2*(K12^2 - k1^2 - k2^2)},
         {k13 -> 1/2*(K13^2 - k1^2 - k3^2)},
         {k23 -> -1/2*(Q23^2 - k2^2 - k3^2)}}]; 

    terminos = filtrarExpandNum[filtrado, {Q23 -> Sqrt[k2^2 + k3^2 - 2*k23]}];

    tablaDen[terminos]
];
*)

(* Mercedes Benz*)

terminosT10[] := terminosDiagrama[
    sinDenT10 /. {k1 -> -k1},
    denT10 /. {k1 -> -k1},
    {{k12 -> 1/2*(K12^2 - k1^2 - k2^2)},
     {k13 -> 1/2*(K13^2 - k1^2 - k3^2)},
     {k23 -> -1/2*(Q23^2 - k2^2 - k3^2)}},
    {Q23 -> Sqrt[k2^2 + k3^2 - 2*k23]}];

tablaT10[] := tablaDen[terminosT10[]];


(* Mercedes Benz ghost1*)

terminosT10ghost1[] := terminosDiagrama[
    sinDenT10ghost1 /. {k1 -> -k1},
    denT10ghost1 /. {k1 -> -k1},
    {{k12 -> 1/2*(K12^2 - k1^2 - k2^2)},
     {k13 -> 1/2*(K13^2 - k1^2 - k3^2)},
     {k23 -> -1/2*(Q23^2 - k2^2 - k3^2)}},
    {Q23 -> Sqrt[k2^2 + k3^2 - 2*k23]}];

tablaT10ghost1[] := tablaDen[terminosT10ghost1[]];

(* Mercedes Benz ghost2*)

terminosT10ghost2[] := terminosDiagrama[
    sinDenT10ghost2 /. {k1 -> -k1},
    denT10ghost2 /. {k1 -> -k1},
    {{k12 -> 1/2*(K12^2 - k1^2 - k2^2)},
     {k13 -> 1/2*(K13^2 - k1^2 - k3^2)},
     {k23 -> -1/2*(Q23^2 - k2^2 - k3^2)}},
    {Q23 -> Sqrt[k2^2 + k3^2 - 2*k23]}];

tablaT10ghost2[] := tablaDen[terminosT10ghost2[]];

(*Earth*)

terminosEarth[] := terminosDiagrama[
    sinDenEarth,
    denEarth,
    {},
    {}];

tablaEarth[] := tablaDen[terminosEarth[]];

(*Tortuga*)

terminosTortuga[] := terminosDiagrama[
    sinDenTortuga,
    denTortuga,
    {},
    {}];

tablaTortuga[] := tablaDen[terminosTortuga[]];

(*Oso*)

terminosOso[] := terminosDiagrama[sinDenOso, denOso,{},{}];
tablaOso[] := tablaDen[terminosOso[]];

(*cilindro*)
(*falta*)
(*cilindro circulante*)
terminosCilCirculante[] := terminosDiagrama[
    sinDenCilCirc /. {k2 -> -k2},
    denCilCirc /. {k2 -> -k2},
    {{k12 -> 1/2*(K12^2 - k1^2 - k2^2)},
     {k23 -> 1/2*(K23^2 - k2^2 - k3^2)}},
    {K12 -> Sqrt[k1^2 + k2^2 + 2*k12],
     K23 -> Sqrt[k2^2 + k3^2 + 2*k23]}];

tablaCilCirculante[] := tablaDen[terminosCilCirculante[]];

(*Cilindro 1loop ghost*)

terminosCil1LoopGhost[] := terminosDiagrama[
    sinDenCil1LoopGhost /. {k2 -> -k2},
    denCil1LoopGhost /. {k2 -> -k2},
    {{k12 -> 1/2*(K12^2 - k1^2 - k2^2)},
     {k23 -> 1/2*(K23^2 - k2^2 - k3^2)},
     {k23 -> -1/2*(Q23^2 - k2^2 - k3^2)}},
    {}];
tablaCil1LoopGhost[] := tablaDen[terminosCil1LoopGhost[]];

(*Cilindro 2loop ghost*)

terminosCil2LoopGhost[] := terminosDiagrama[
    sinDenCil2LoopGhost /. {k2 -> -k2},
    denCil2LoopGhost /. {k2 -> -k2},
    {{k12 -> 1/2*(K12^2 - k1^2 - k2^2)},
     {k23 -> 1/2*(K23^2 - k2^2 - k3^2)},
     {k23 -> -1/2*(Q23^2 - k2^2 - k3^2)}},
    {}];
tablaCil2LoopGhost[] := tablaDen[terminosCil2LoopGhost[]];

(*Anteojo*)

terminosAnteojo[] := terminosDiagrama[
    sinDenAnteojo,
    denAnteojo,
    {{k23 -> 1/2*(K23^2 - k2^2 - k3^2)},
     {k12 -> 1/2*(K123^2 - K23^2 - k1^2) - k13}},
    {K123 -> Sqrt[k1^2 + k2^2 + k3^2 + 2*k12 + 2*k13 + 2*k23],
     K23 -> Sqrt[k2^2 + k3^2 + 2*k23]}];

tablaAnteojo[] := tablaDen[terminosAnteojo[]];

(*Muneco nieve*)
terminosMunecoNieve[] := terminosDiagrama[
    sinDenMunecoNieve,
    denMunecoNieve,
    {{k12 -> 1/2*(K12^2 - k1^2 - k2^2)}},
    {}];

tablaMunecoNieve[] := tablaDen[terminosMunecoNieve[]];


(*Muneco nieve ghost*)
terminosMunecoNieveGhost[] := terminosDiagrama[
    sinDenMunecoNieveGhost,
    denMunecoNieveGhost,
    {{k13 -> 1/2*(K13^2 - k1^2 - k3^2)}},
    {}];

tablaMunecoNieveGhost[] := tablaDen[terminosMunecoNieveGhost[]];
