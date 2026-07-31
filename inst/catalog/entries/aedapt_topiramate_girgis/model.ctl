$PROBLEM Topiramate - Girgis population PK
$SUBROUTINES ADVAN4 TRANS4
$INPUT ID TIME EVID AMT RATE II SS CMT DV MDV WT AGE ADJUNCTIVE INDUCER COMED_VPA NONINDUCER
$DATA data.csv IGNORE=@
$THETA
 (0, 1.21, 1210) ; base CL/F
 (0, 0.479, 479) ; adjunctive effect
 (0, 0.453, 453) ; CL weight exponent
 (-0.0306, -0.00306, 0) ; CL age coefficient
 (0, 1.94, 1940) ; inducer factor
 (0, 0.686, 686) ; valproate factor
 (0, 0.635, 635) ; non-inducer factor
 (0, 4.61, 4610) ; V1/F
 (0, 1.14, 1140) ; V1 weight exponent
 (0, 0.105, 105) ; KA
 (0, 0.577, 577) ; K12
 (0, 0.0586, 58.6) ; K21
$OMEGA
 0.0744
 1.35
 0.0499
$SIGMA
 0.06482116
 0.03229209
$PK
CL=THETA(1)*(1+ADJUNCTIVE*THETA(2))*(WT/69.9)^THETA(3)*exp(THETA(4)*(AGE-31.4))*(THETA(5)^INDUCER)*(THETA(6)^COMED_VPA)*(THETA(7)^NONINDUCER)*exp(ETA(1))
V1=THETA(8)*(WT/69.9)^THETA(9)*exp(ETA(2))
Q=THETA(11)*V1
V2=V1*THETA(11)/THETA(12)
KA=THETA(10)*exp(ETA(3))
S2=V1
S3=V2
$ERROR
Y=F*(1+ERR(1))+ERR(2)
$TABLE ID TIME DV PRED IPRED CWRES NOPRINT ONEHEADER
