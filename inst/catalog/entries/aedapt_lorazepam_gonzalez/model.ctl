$PROBLEM Lorazepam - Gonzalez population PK
$SUBROUTINES ADVAN4 TRANS4
$INPUT ID TIME EVID AMT RATE II SS CMT DV MDV WT AGE
$DATA data.csv IGNORE=@
$THETA
 (0, 0.879, 879) ; V1/F per kg
 (0, 0.115, 115) ; CL/F coefficient
 (0, 0.542, 542) ; V2/V1 ratio
 (0, 1.45, 1450) ; Q/F
 (0, 0.133, 133) ; age exponent
 (0, 1, 1000) ; KA
$OMEGA
 0.2
 0.15
 0.28
$SIGMA
 0.1
$PK
V1=THETA(1)*WT*exp(ETA(1))
CL=THETA(2)*(WT^0.75)*(AGE/4.7)^THETA(5)*exp(ETA(2))
V2=THETA(3)*THETA(1)*WT
Q=THETA(4)
KA=THETA(6)*exp(ETA(3))
S2=V1
S3=V2
$ERROR
Y=F*(1+ERR(1))
$TABLE ID TIME DV PRED IPRED CWRES NOPRINT ONEHEADER
