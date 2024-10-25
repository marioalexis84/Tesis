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

