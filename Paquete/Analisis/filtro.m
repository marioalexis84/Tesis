filtrarExpandSeq[paso_, reemp_List] := Module[
    {p, i},
    p = paso; 
    For[i = 1, i <= Length[reemp], i++ , 
    p = filtrarExpand[p /. reemp[[i]], 7];];

    (* return ultimo paso del loop *)
    p
];

filtrarExpandNum[paso_, reemp_List] := Module[
    {num, term},
    num = ExpandAll[Numerator[paso] /. reemp]; 
    term = filtrarExpand[num/Denominator[paso], 7];

    term
];

terminosDiagrama[sinDen_, den_, reemp_List, reempNum_List] := Module[
    {paso, filtrado, terminos},

    paso = Filtros[sinDen, den, D, 7];
    filtrado = filtrarExpandSeq[paso, reemp]; 
    terminos = filtrarExpandNum[filtrado, reempNum];

    terminos
];

tablaDen[term_] := Module[
    {pot, tab}, 
    pot = ListAlgebraicDegree[
        #, {k1, k2, k3, K12, K13, K23, K123, Q23}] & /@ Denominator[term];

    tab = DeleteCases[#, 0] & /@ 
      Table[{k1, k2, k3, K12, K13, K23, K123, Q23} * pot[[n]]/2,
            {n, 1, Length[pot]}] /. {K12 -> k1 + k2, K13 -> k1 + k3, K23 -> k2 + k3,
        Q23 -> k2 - k3, K123 -> k1 + k2 + k3};

    tab
];

mascaraTablaLen4[tab_] := Flatten[Position[tab, s_ /; Length[s] == 4]];

