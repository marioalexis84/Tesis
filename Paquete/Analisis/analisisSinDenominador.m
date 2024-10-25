(*<FeynCalc`;
*)

AlgebraicDegree[eqn_, vars_List] :=
    Max[Total[
        GroebnerBasis`DistributedTermsList[eqn /. Equal :> Subtract, 
            vars][[1, All, 1]], {2}
        ]
    ];


AlgebraicDegree[eqn_] := AlgebraicDegree[eqn, Variables[eqn]];

funcGra[lis_] := AlgebraicDegree[Denominator[#]] & /@ lis;


filtroGra[lis_, deg_] := 
    lis[[#]] & /@ Flatten[Position[funcGra[lis], _?(# > deg &) ]];


Filtros[file_, den_, dim_, degree_] := Module[
    {sinDen, sinDenDim, numExpand, lista},

    sinDen = Get[file];

    (* sacamos el factor (D-2) de denominador para evitar que lo expanda *)
    sinDen = sinDen*(D-2)^2;

    (* limpiamos posibles FeynCalc scalarProducts activados *)
    FCClearScalarProducts[];
    FCSetScalarProducts[
        {SPD[k1], SPD[k2], SPD[k3], SPD[k1, k2], SPD[k1, k3], SPD[k2, k3]},
        {k1^2, k2^2, k3^2, k12, k13, k23}
    ];

    (* aplicamos valor especifico para la dimension D*)
    sinDenDim = sinDen /. D -> dim;
    numExpand = ExpandAll[sinDenDim];
(*
Print[numExpand];
*)
    lista = List @@ Distribute[numExpand*den];
(*
Print["lista", lista];
*)

    grados = AlgebraicDegree[Denominator[#]]& /@ lista;

Print[grados];

    paso0 = lista[[#]]& /@ Flatten[Position[grados, _?(# > 4 &)]];
    (* esto quiza simplifica algo *)
    paso1 = Together[Total[paso0]];
    paso2 = List @@ Distribute[paso1];
    paso2
(*
    paso3 =   ExpandAll[
            paso2 /. {k23 -> 1/2*(K23^2 - k2^2 - k3^2)}
        ];

(*    paso3 = Distribute[# ] & /@ (
        ExpandAll[
            paso2 /. {k23 -> 1/2*(K23^2 - k2^2 - k3^2)}
        ]
    );
*)

    paso4 = List @@ Total[paso3];


    paso5 = filtroGra[paso4, degree];

(*    paso6 = Distribute[# ] & /@ (
*)
    paso6 =
        ExpandAll[
            paso5 /. {k12 -> 1/2*(K123^2 - K23^2 - k1^2) - k13}
        ];
(*    );
*)
    paso7 = List @@ Total[paso6];

    paso8 = filtroGra[paso7, degree];

    factorDim = 1/(dim-2)^2;

    FCClearScalarProducts[];
    paso8*factorDim
*)
]

reemplazoMomentos[feynDenominator_] := 1/Times @@ (List @@ FeynAmpDenominatorCombine[feynDenominator]) /.
    {PropagatorDenominator[Momentum[k1, D], 0] -> k1^2,
     PropagatorDenominator[Momentum[k2, D], 0] -> k2^2,
     PropagatorDenominator[Momentum[k3, D], 0] -> k3^2,
     PropagatorDenominator[Momentum[k1, D] + Momentum[k2, D], 0] -> K12^2,
     PropagatorDenominator[Momentum[k1, D] + Momentum[k3, D], 0] -> K13^2,
     PropagatorDenominator[Momentum[k2, D] + Momentum[k3, D], 0] -> K23^2,
     PropagatorDenominator[Momentum[k1, D] + Momentum[k2, D] + Momentum[k3, D], 0] -> K123^2
    };     

(*
resultado = Filtro["/home/alexis/Descargas/TesisMP/Diagramas/SinDenominador/\
AnteojoFinal_sinDenominadork1.txt", den, 3, 7]
*)
