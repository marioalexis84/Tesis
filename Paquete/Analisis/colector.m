
(*Esta funcion aplica a la lista de elementos de tipo k, en el que el \
elemento distintivo, de grado 4, tiene un factor 2.}
ej X={2*k1 -2*k2,k1,k3} quiere decir (k1-k2)^4*k1^2*k3^2. Y la \
funcion devuelve las sustituciones correspondientes*)

SustK[x_] := Module[{cuarta, cuartapos, listacortada, Cambios},
   cuarta[X_] := 
    Flatten[Select[Coefficient[#, {k1, k2, k3}] & /@ X, 
      ContainsAny[{2, -2}]]];
   cuartapos[X_] := 
    Position[Coefficient[#, {k1, k2, k3}] & /@ X, cuarta[X]];
   listacortada[X_] := Drop[X, Flatten[cuartapos[X]]];
   Cambios[X_] := 
    Flatten[Solve[
       Join[listacortada[
          x], {1/2*X[[Flatten[cuartapos[x]][[1]]]]}] == {K1, K2, 
         K3}, {k1, k2, k3}]] /. {K1 -> k1, K2 -> k2, K3 -> k3};
   (*Cambios[X_]=Flatten[Solve[Join[listacortada[X],{X[[Flatten[
   cuartapos[X]][[1]]]]}]=={K2,K3,K1},{k1,k2,k3}]]*);
   Cambios[x]];

(*Aqui x es la lista de k que aparece en el denominador. Salida 1 \
corresponde a I. Salida 0 corresponde a J*)
(*Necesitaremos dos \
tablas: la de terminos completos y la de denominadores*)

IoJ[x_] := Module[{vectorizacion, coef, Clasificador, Etiqueta},
   vectorizacion[X_] := 
    X /. {k1 -> {1, 0, 0}, k2 -> {0, 1, 0}, k3 -> {0, 0, 1}};
   (*coef encuentra los coeficientes de la combinacion de los 3 \
primeros que da el cuarto*)
   
   coef[X_] := Flatten[NullSpace[Transpose[vectorizacion[X]]]];
   Clasificador[X_] := Length[Position[X, y_ /; y == 0]];
   Etiqueta[X_] := If[Clasificador[coef[X]] == 0, 1, 0];
   Etiqueta[x]];

(*Aqui "y" es la tabla de terminos e "yt" la tabla de denominadores. \
Selector simplemente genera una tabla con los elementos filtrados por \
ser I o J. Puede ser la tabla de terminos o la de denominadores . \
Para ese ultimo caso hay que hacer SelectorIoJ[yt,yt,a]*)


SelectorIoJ[y_, yt_, a_] := 
  y[[#]] & /@ Flatten[Position[IoJ[#] & /@ yt, s_ /; s == a]];


SustI[x_] := Module[{vectorizacion, coef, Changelist0, Changelist},
   vectorizacion[X_] := 
    X /. {k1 -> {1, 0, 0}, k2 -> {0, 1, 0}, k3 -> {0, 0, 1}};
   (*coef encuentra los coeficientes de la combinacion de los 3 \
primeros que da el cuarto*)
   
   coef[X_] := 
    LinearSolve[Transpose[Drop[vectorizacion[X], -1]], 
     vectorizacion[X][[Length[X]]]];
   Changelist0[X_] := coef[X]*Drop[X, -1];
   (*aqui se invierte la relacion. Es decir, 
   se hallan los k en terminos de los K mayusculas a los que luego le \
vuelvo a cambiar el nombre*)
   
   Changelist[X_] := 
    Flatten[Solve[
       Changelist0[X] == {K1, K2, K3}, {k1, k2, k3}]] /. {K1 -> k1, 
      K2 -> k2, K3 -> k3};
   Changelist[x]];


SustJ[x_] := Module[{vectorizacion, coef, reduc, coefcrop, Changelist},
   vectorizacion[X_] := 
    X /. {k1 -> {1, 0, 0}, k2 -> {0, 1, 0}, k3 -> {0, 0, 1}};
   (*coef encuentra los coeficientes de la combinacion de los 3 \
primeros que da el cuarto*)
   
   coef[X_] := Flatten[NullSpace[Transpose[vectorizacion[X]]]];
   reduc[X_] := Delete[X, Position[coef[X], y_ /; y == 0]];
   coefcrop[X_] := DeleteCases[coef[X], 0];
   Changelist[X_] := 
    Flatten[Solve[
       Join[Drop[coefcrop[X], -1]*
          Drop[reduc[X], -1], {X[[
           Flatten[Position[coef[X], y_ /; y == 0]][[1]]]]}] == {-1/
           Last[coefcrop[X]]*K1, -1/Last[coefcrop[X]]*K2, K3}, {k1, 
        k2, k3}]] /. {K1 -> k1, K2 -> k2, K3 -> k3};
   (*aqui se invierte la relacion. Es decir, 
   se hallan los k en terminos de los K mayusculas a los que luego le \
vuelvo a cambiar el nombre*)
   Changelist[x]];


(*Aqui x es la tabla de terminos e y la de los denominadores. \
Jterms  e Iterms muestra los terminos pero habiando reescrito los K \
mayusculas si los hubiera. Para los Kterms, como no una tabla exclusiva, no necesito una segunda variable*)

Kterms[x_] := (ExpandAll[
     Numerator[x] /. {K12 -> Sqrt[k1^2 + k2^2 + 2*k12], 
       K13 -> Sqrt[k1^2 + k3^2 + 2*k13], 
       K23 -> Sqrt[k2^2 + k3^2 + 2*k23], 
       Q23 -> Sqrt[ k2^2 + k3^2 - 2*k23], 
       K123 -> Sqrt[
         k1^2 + k2^2 + k3^2 + 2*k12 + 2*k13 + 
          2*k23]}])*1/(Denominator[x] /. {K12 -> k1 + k2, 
       K13 -> k1 + k3, K23 -> k2 + k3, Q23 -> k2 - k3, K123 -> k1 + k2 + k3});
Jterms[x_, y_] := (ExpandAll[
     Numerator[
       SelectorIoJ[x, y, 0]] /. {K12 -> Sqrt[k1^2 + k2^2 + 2*k12], 
       K13 -> Sqrt[k1^2 + k3^2 + 2*k13], 
       K23 -> Sqrt[k2^2 + k3^2 + 2*k23], 
       Q23 -> Sqrt[ k2^2 + k3^2 - 2*k23], 
       K123 -> Sqrt[
         k1^2 + k2^2 + k3^2 + 2*k12 + 2*k13 + 
          2*k23]}])*1/(Denominator[
       SelectorIoJ[x, y, 0]] /. {K12 -> k1 + k2, K13 -> k1 + k3, K23 -> k2 + k3,
       Q23 -> k2 - k3, K123 -> k1 + k2 + k3});


Iterms[x_, y_] := (ExpandAll[
     Numerator[
       SelectorIoJ[x, y, 1]] /. {K12 -> Sqrt[k1^2 + k2^2 + 2*k12], 
       K13 -> Sqrt[k1^2 + k3^2 + 2*k13], 
       K23 -> Sqrt[k2^2 + k3^2 + 2*k23], 
       Q23 -> Sqrt[ k2^2 + k3^2 - 2*k23], 
       K123 -> Sqrt[
         k1^2 + k2^2 + k3^2 + 2*k12 + 2*k13 + 
          2*k23]}])*1/(Denominator[
       SelectorIoJ[x, y, 1]] /. {K12 -> k1 + k2, K13 -> k1 + k3, K23 -> k2 + k3,
       Q23 -> k2 - k3, K123 -> k1 + k2 + k3});



(*Esta funcion hace el laburo de sustituir productos escalares en \
terminos de las k1,k2,k3 finales  que todo aquel producto de una \
variable consigo misma dara cero*)


SustSP[a_] := {k1 -> Sqrt[ExpandAll[(k1*k1 /. a)]] /. {k1^2 -> 0, 
    k2^2 -> 0, k3^2 -> 0, k1*k2 -> k12, k1*k3 -> k13, k2*k3 -> k23}, 
  k2 -> Sqrt[ExpandAll[(k2*k2 /. a)]] /. {k1^2 -> 0, k2^2 -> 0, 
    k3^2 -> 0, k1*k2 -> k12, k1*k3 -> k13, k2*k3 -> k23}, 
  k3 -> Sqrt[ExpandAll[(k3*k3 /. a)]] /. {k1^2 -> 0, k2^2 -> 0, 
    k3^2 -> 0, k1*k2 -> k12, k1*k3 -> k13, k2*k3 -> k23}, 
  k12 -> ExpandAll[(k1*k2 /. a)] /. {k1^2 -> 0, k2^2 -> 0, k3^2 -> 0, 
    k1*k2 -> k12, k1*k3 -> k13, k2*k3 -> k23}, 
  k13 -> ExpandAll[(k1*k3 /. a)] /. {k1^2 -> 0, k2^2 -> 0, k3^2 -> 0, 
    k1*k2 -> k12, k1*k3 -> k13, k2*k3 -> k23}, 
  k23 -> ExpandAll[(k2*k3 /. a)] /. {k1^2 -> 0, k2^2 -> 0, k3^2 -> 0, 
    k1*k2 -> k12, k1*k3 -> k13, k2*k3 -> k23}};


(*Une las dos sustituciones. x aqui es la tabla de denominadores. El \
out de la funcion es la sustitucion de productos como k1^2 o k12, \
cuando se redefinen las variables. Ejemplo: si k1->k1+k2, k1^2->2k12, \
modulo cosas que son cero*)


SustI2[x_] := SustSP[SustI[x]];
SustJ2[x_] := SustSP[SustJ[x]];
SustK2[x_] := SustSP[SustK[x]];



(*Llegamos a las funciones casi finales. Ican y Jcan y Kcan, por "canonicos"*)

Ican[x_, y_] := 
  Table[(Numerator[Iterms[x, y]][[
       i]] /. (SustI2[#] & /@ SelectorIoJ[y, y, 1])[[
       i]])*1/(Denominator[Iterms[x, y]][[
        i]] /. (SustI[#] & /@ SelectorIoJ[y, y, 1])[[i]]), {i, 1, 
    Length[Iterms[x, y]]}];

Jcan[x_, y_] := 
 Table[(Numerator[Jterms[x, y]][[
      i]] /. (SustJ2[#] & /@ SelectorIoJ[y, y, 0])[[
      i]])*1/(Denominator[Jterms[x, y]][[
       i]] /. (SustJ[#] & /@ SelectorIoJ[y, y, 0])[[i]]), {i, 1, 
   Length[Jterms[x, y]]}];

Kcan[x_, y_] := 
  Table[(Numerator[Kterms[x]][[i]] /. (SustK2[#] & /@ y)[[
       i]])*1/(Denominator[Kterms[x]][[i]] /. (SustK[#] & /@ y)[[
        i]]), {i, 1, Length[Kterms[x]]}];

(*Funcion Principal. Falta juntar todo y sustituir cosas segun \
identidades de regularizacion dimensional. recordemos que hay dos \
tipos de J*)


Icanonico[x_, y_] := 
 Factor[Together[Total[Ican[x, y]]] /. {k1^2 -> 0, k2^2 -> 0, k3^2 -> 0, k1^4 -> 0, k2^4 -> 0, k3^4 -> 0, k12^2 -> -2, k23^2 -> -2, 
    k13^2 -> -2, k12 -> 1, k13 -> 1, k23 -> 1}];

(*recordar lo que pasaba en las J. si habia un solo k12 daba cero. Los distintos de cero son aquellos: 
      J1) que tuvieran k13^2 o k23^3, ambos iguales o J2) k13*k12.*)

Jcanonico[x_, y_] := 
  Factor[Together[Total[Jcan[x, y]]] /. {k1^2 -> 0, k2^2 -> 0, k3^2 -> 0, k1^4 -> 0, k2^4 -> 0, k3^4 -> 0, k12 -> 0, k13^2 -> J1, 
     k23^2 -> J1, k13*k23 -> -1/2*J1}];
Kcanonico[x_, y_] := 
  Factor[Together[Total[Kcan[x, y]]] /. {k1^2 -> 0, k2^2 -> 0, k3^2 -> 0, k1^4 -> 0, k2^4 -> 0, k3^4 -> 0, k12^2 -> K1, k13^2 -> K2, 
     k23^2 -> K2, k12 -> 0, k13 -> 0}];
     
