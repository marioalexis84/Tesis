denAnteojo = FeynAmpDenominator[
    PropagatorDenominator[Momentum[k2, D], 0],
    PropagatorDenominator[Momentum[k3, D], 0],
    PropagatorDenominator[Momentum[k1, D], 0],
    PropagatorDenominator[Momentum[k2, D] + Momentum[k3, D], 0],
    PropagatorDenominator[Momentum[k2, D] + Momentum[k3, D] + Momentum[k1, D], 0]];

denCilindro = FeynAmpDenominator[
    PropagatorDenominator[Momentum[k1, D], 0],
    PropagatorDenominator[Momentum[k1, D], 0],
    PropagatorDenominator[Momentum[k2, D], 0],
    PropagatorDenominator[Momentum[k3, D], 0],
    PropagatorDenominator[Momentum[k1, D] + Momentum[k2, D], 0],
    PropagatorDenominator[Momentum[k1, D] + Momentum[k3, D], 0]];


(*falta tortuga*)

denEarth = FeynAmpDenominator[PropagatorDenominator[Momentum[k1, D], 0]]\
    * FeynAmpDenominator[PropagatorDenominator[Momentum[k2, D], 0]]\
    * FeynAmpDenominator[PropagatorDenominator[Momentum[k3, D], 0]]\
    * FeynAmpDenominator[PropagatorDenominator[Momentum[k1 + k2 + k3, D], 0]];

denMunecoNieve = FeynAmpDenominator[PropagatorDenominator[Momentum[k1, D], 0]]^2\
    * FeynAmpDenominator[PropagatorDenominator[Momentum[-k1 - k2, D], 0]]\
    * FeynAmpDenominator[PropagatorDenominator[Momentum[k2, D], 0]]\
    * FeynAmpDenominator[PropagatorDenominator[Momentum[k3, D], 0]];

denT10 = FeynAmpDenominator[
    PropagatorDenominator[Momentum[k1, D], 0],
    PropagatorDenominator[Momentum[k2, D], 0],
    PropagatorDenominator[Momentum[k3, D], 0],
    PropagatorDenominator[Momentum[k1, D] - Momentum[k2, D], 0],
    PropagatorDenominator[Momentum[k1, D] - Momentum[k3, D], 0],
    PropagatorDenominator[Momentum[k2, D] - Momentum[k3, D], 0]];
