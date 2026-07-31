# Import the antiseizure-medicine models retained by the legacy AEDapt app into
# LibeRary's packaged catalogue. The resulting records are research candidates:
# catalogue inclusion does not confer clinical qualification.
#
# Run from the consolidated LibeR root:
#   Rscript LibeRary/tools/import-aedapt-models.R
#
# Override the source location with AEDAPT_ROOT when migrating another copy.

root <- normalizePath(
  if (file.exists("LibeRary/DESCRIPTION")) "." else
    file.path(dirname(sys.frame(1)$ofile), "..", ".."),
  winslash = "/", mustWork = TRUE
)
package_root <- file.path(root, "LibeRary")
catalog_root <- file.path(package_root, "inst", "catalog")
entries_root <- file.path(catalog_root, "entries")
reference_root <- file.path(
  root, "validation", "liberary", "aed-pkpd-reference", "0.2.2", "models"
)
aedapt_root <- Sys.getenv(
  "AEDAPT_ROOT",
  "C:/Users/svdijkman.DESKTOP-4OG10M4/Desktop/backup_2026/PC_old1/Desktop/aedapt"
)
aedapt_library <- file.path(aedapt_root, "library")

if (!requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("digest", quietly = TRUE)) {
  stop("jsonlite and digest are required.")
}
if (!dir.exists(aedapt_library)) {
  stop("AEDapt model library not found: ", aedapt_library)
}
dir.create(entries_root, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x
lines <- function(...) unlist(list(...), use.names = FALSE)
th <- function(value, name, lower = NULL, upper = NULL, fixed = FALSE) {
  if (is.null(lower)) lower <- if (value > 0) 0 else value * 10
  if (is.null(upper)) upper <- if (value > 0) value * 1000 else 0
  sprintf(
    " (%s, %.12g, %s)%s ; %s",
    format(lower, scientific = FALSE, trim = TRUE),
    value,
    format(upper, scientific = FALSE, trim = TRUE),
    if (isTRUE(fixed)) " FIX" else "",
    name
  )
}

control <- function(title, advan, trans = NULL, input, theta, omega, sigma,
                    pk, error, model = NULL, des = NULL, notes = character(),
                    direct = FALSE) {
  records <- c(
    paste("$PROBLEM", title),
    paste(
      "$SUBROUTINES",
      paste0("ADVAN", advan),
      if (!is.null(trans)) paste0("TRANS", trans) else ""
    ),
    paste("$INPUT ID TIME EVID AMT RATE II SS CMT DV MDV", paste(input, collapse = " ")),
    "$DATA data.csv IGNORE=@"
  )
  if (length(model)) records <- c(records, "$MODEL", paste0("  ", model))
  records <- c(
    records,
    "$THETA", theta,
    omega,
    sigma,
    if (isTRUE(direct)) "$PRED" else "$PK", pk
  )
  if (length(des)) records <- c(records, "$DES", des)
  c(
    records,
    "$ERROR", error,
    "$TABLE ID TIME DV PRED IPRED CWRES NOPRINT ONEHEADER",
    if (length(notes)) paste0("; MIGRATION_NOTE: ", notes) else character()
  )
}

diag_omega <- function(...) c("$OMEGA", sprintf(" %.12g", c(...)))
block_omega <- function(size, values) c(
  paste0("$OMEGA BLOCK(", size, ")"),
  sprintf(" %.12g", values)
)
sigma <- function(...) c("$SIGMA", sprintf(" %.12g", c(...)))

spec <- function(id, source, drug, author, age, population, units, ctl,
                 covariates = character(), optional = character(),
                 reference_id = "", pmid = "", corrections = character(),
                 assumptions = character(), title = "", model_type = "pk",
                 advan = 2L, trans = 2L, use_ode = FALSE,
                 version = "1.0.0") {
  list(
    id = id, source = source, drug = drug, author = author, age = age,
    population = population, units = units, ctl = ctl,
    covariates = covariates, optional = optional,
    reference_id = reference_id, pmid = pmid,
    corrections = corrections, assumptions = assumptions,
    title = title, model_type = model_type,
    advan = advan, trans = trans, use_ode = use_ode,
    version = version
  )
}

specs <- list(
  spec(
    "aedapt_cbz_jiao", "CBZ_AP_JIAO.R", "carbamazepine", "Jiao",
    c(1.2, 85.1), "Chinese people with epilepsy", "mg/L",
    control(
      "Carbamazepine - Jiao population PK", 2, 2,
      c("WT", "AGE", "DOSE_MG_KG_DAY", "COMED_PHT", "COMED_PHB",
        "COMED_VPA_HIGH"),
      c(
        th(.0734, "base CL/F"), th(.406, "dose exponent"),
        th(.694, "weight exponent"), th(1.45, "phenytoin factor"),
        th(1.17, "phenobarbital factor"), th(1.21, "high-dose valproate factor"),
        th(1.91, "age over 65 factor"), th(.849, "V/F per kg"),
        th(1.2, "KA")
      ),
      diag_omega(.0254, .010), sigma(.975),
      lines(
        "ELDERLY=ifelse(AGE>65,1,0)",
        "CL=THETA(1)*(DOSE_MG_KG_DAY^THETA(2))*(WT^THETA(3))*(THETA(4)^COMED_PHT)*(THETA(5)^COMED_PHB)*(THETA(6)^COMED_VPA_HIGH)*(THETA(7)^ELDERLY)*exp(ETA(1))",
        "V=THETA(8)*WT*exp(ETA(2))", "KA=THETA(9)", "S2=V"
      ),
      "Y=F+ERR(1)"
    ),
    covariates = c("WT", "AGE", "DOSE_MG_KG_DAY"),
    optional = c("COMED_PHT", "COMED_PHB", "COMED_VPA_HIGH"),
    reference_id = "aed_cbz_12766553_m01", pmid = "12766553",
    corrections = c(
      "Restored V/F=0.849*WT; AEDapt had substituted 1.5*WT.",
      "Corrected the AEDapt THETA7/THETA8 label and implementation swap.",
      "Retained OMEGA values as variances; no CV or SD reconversion."
    ),
    title = "Carbamazepine population PK - Jiao et al.", advan = 2, trans = 2
  ),
  spec(
    "aedapt_cbz_delgado", "CBZ_P_DELGADO.R", "carbamazepine", "Delgado",
    c(.5, 15), "Paediatric people with epilepsy", "mg/L",
    control(
      "Carbamazepine - Delgado population PK", 2, 2,
      c("WT", "AGE", "DOSE_MG_KG_DAY", "COMED_PHB"),
      c(
        th(.0122, "weight coefficient"), th(.0467, "dose coefficient"),
        th(.331, "age exponent"), th(.289, "phenobarbital effect"),
        th(.65, "KA"), th(1.5, "V/F per kg"), th(.85, "F1")
      ),
      diag_omega(.118), sigma(1.52),
      lines(
        "CL=(THETA(1)*WT+THETA(2)*DOSE_MG_KG_DAY)*(AGE^THETA(3))*(1+THETA(4)*COMED_PHB)*exp(ETA(1))",
        "KA=THETA(5)", "V=THETA(6)*WT", "F1=THETA(7)", "S2=V"
      ),
      "Y=F+ERR(1)"
    ),
    covariates = c("WT", "AGE", "DOSE_MG_KG_DAY"),
    optional = "COMED_PHB",
    reference_id = "aed_cbz_9108639_m01", pmid = "9108639",
    corrections = "Corrected AEDapt's fourth-root OMEGA error: 0.118 is the NONMEM variance.",
    title = "Carbamazepine population PK - Delgado et al.", advan = 2, trans = 2
  ),
  spec(
    "aedapt_clobazam_lamba", "CLBZ_AP_LAMBA.R", "clobazam", "Lamba",
    c(1, 23), "Predominantly White paediatric and young-adult population", "mcg/L",
    control(
      "Clobazam - Lamba population PK", 2, 2, "WT",
      c(th(5.99, "CL/F at 70 kg"), th(96.8, "V/F"), th(2.5, "KA")),
      diag_omega(.221841, .40), sigma(.04),
      lines(
        "CL=THETA(1)*(WT/70)^0.75*exp(ETA(1))",
        "V=THETA(2)*exp(ETA(2))", "KA=THETA(3)", "S2=V*0.001"
      ),
      "Y=F*(1+ERR(1))"
    ),
    covariates = "WT",
    assumptions = "A 20% proportional residual-error SD (SIGMA=0.04) was added because AEDapt did not retain a residual model.",
    title = "Clobazam population PK - Lamba legacy model", advan = 2, trans = 2
  ),
  spec(
    "aedapt_clobazam_saruwatari", "CLBZ_AP_SARUWATARI.R", "clobazam", "Saruwatari",
    c(1, 52), "Japanese people with epilepsy", "mcg/L",
    control(
      "Clobazam - Saruwatari population PK", 2, 2,
      c("WT", "COMED_ZNS", "COMED_PHB", "COMED_PHT", "CYP2C19_HET",
        "CYP2C19_PM", "POR28_CT", "POR28_TT"),
      c(
        th(.0594, "KA"), th(13.3, "V/F coefficient"),
        th(.136, "V/F weight exponent"), th(.511, "CL/F coefficient"),
        th(.54, "CL/F weight exponent"), th(.484, "zonisamide factor"),
        th(1.66, "phenobarbital factor"), th(1.93, "phenytoin factor"),
        th(.944, "CYP2C19 heterozygote factor"),
        th(.819, "CYP2C19 poor-metaboliser factor"),
        th(1.02, "POR*28 CT factor"), th(1.44, "POR*28 TT factor")
      ),
      diag_omega(1.91e-5, .0669, 3.6e-9), sigma(.107),
      lines(
        "KA=THETA(1)*exp(ETA(1))",
        "CL=THETA(4)*(WT^THETA(5))*(THETA(6)^COMED_ZNS)*(THETA(7)^COMED_PHB)*(THETA(8)^COMED_PHT)*(THETA(9)^CYP2C19_HET)*(THETA(10)^CYP2C19_PM)*(THETA(11)^POR28_CT)*(THETA(12)^POR28_TT)*exp(ETA(2))",
        "V=THETA(2)*(WT^THETA(3))*exp(ETA(3))", "S2=V*0.001"
      ),
      "Y=F*(1+ERR(1))"
    ),
    covariates = c("WT", "CYP2C19_HET", "CYP2C19_PM", "POR28_CT", "POR28_TT"),
    optional = c("COMED_ZNS", "COMED_PHB", "COMED_PHT"),
    corrections = "Removed AEDapt's random genotype generation; observed patient genotypes are required.",
    title = "Clobazam population PK - Saruwatari legacy model", advan = 2, trans = 2
  ),
  spec(
    "aedapt_clonazepam_yukawa", "CLNZ_AP_YUKAWA.R", "clonazepam", "Yukawa",
    c(1, 20), "Japanese paediatric people with epilepsy", "mcg/L",
    control(
      "Clonazepam - Yukawa steady-state model", 2, 2,
      c("WT", "DAILY_DOSE", "COMED_VPA", "COMED_CBZ"),
      c(
        th(.144, "base CL/F"), th(.828, "weight exponent"),
        th(1.14, "valproate factor"), th(1.22, "carbamazepine factor")
      ),
      diag_omega(.0404), sigma(.0346),
      lines(
        "CL=THETA(1)*(WT^THETA(2))*(THETA(3)^COMED_VPA)*(THETA(4)^COMED_CBZ)*exp(ETA(1))",
        "F=1000*DAILY_DOSE/(24*CL)"
      ),
      "Y=F+ERR(1)",
      direct = TRUE
    ),
    covariates = c("WT", "DAILY_DOSE"),
    optional = c("COMED_VPA", "COMED_CBZ"),
    reference_id = "aed_clnz_14651674_m01", pmid = "14651674",
    assumptions = "The retained AEDapt model predicts average steady-state concentration only; no V/F or KA was available for dynamic forecasting.",
    corrections = "Recorded the discrepancy between AEDapt's Yukawa attribution and the mapped appendix reference for independent review.",
    title = "Clonazepam steady-state PK - Yukawa legacy model", advan = 2, trans = 2
  ),
  spec(
    "aedapt_lamotrigine_rivas", "LMT_A_RIVAS.R", "lamotrigine", "Rivas",
    c(16, 68), "Adult people with epilepsy", "mg/L",
    control(
      "Lamotrigine - Rivas population PK", 2, 2,
      c("WT", "COMED_VPA", "COMED_PHT", "COMED_PHB", "COMED_CBZ"),
      c(
        th(.028, "CL/F per kg"), th(-.713, "valproate log effect"),
        th(.663, "phenytoin log effect"), th(.588, "phenobarbital log effect"),
        th(.467, "carbamazepine log effect"), th(.864, "multiple-inducer log effect"),
        th(1.5, "V/F per kg"), th(1.3, "KA")
      ),
      diag_omega(.07285), sigma(1.25),
      lines(
        "NIND=COMED_PHT+COMED_PHB+COMED_CBZ",
        "IND=ifelse(NIND>1,1,0)",
        "CL=THETA(1)*WT*exp(THETA(2)*COMED_VPA+THETA(3)*COMED_PHT+THETA(4)*COMED_PHB+THETA(5)*COMED_CBZ+THETA(6)*IND+ETA(1))",
        "V=THETA(7)*WT", "KA=THETA(8)", "S2=V"
      ),
      "Y=F+ERR(1)"
    ),
    covariates = "WT",
    optional = c("COMED_VPA", "COMED_PHT", "COMED_PHB", "COMED_CBZ"),
    reference_id = "aed_lmt_18641550_m01", pmid = "18641550",
    corrections = "Restored the individual comedication effects that were overwritten by a second TVCL assignment in AEDapt.",
    title = "Lamotrigine population PK - Rivas et al.", advan = 2, trans = 2
  ),
  spec(
    "aedapt_lamotrigine_he", "LMT_P_HE.R", "lamotrigine", "He",
    c(1, 18), "Chinese paediatric people with epilepsy", "mg/L",
    control(
      "Lamotrigine - He population PK", 2, 2,
      c("WT", "COMED_VPA", "COMED_CBZ", "COMED_PHB"),
      c(
        th(1.01, "base CL/F"), th(.635, "weight exponent"),
        th(-.753, "valproate log effect"), th(.868, "carbamazepine log effect"),
        th(.633, "phenobarbital log effect"),
        th(16.7, "V/F at 27.87 kg"),
        th(1, "KA")
      ),
      diag_omega(.067), sigma(.045),
      lines(
        "CL=THETA(1)*(WT/27.87)^THETA(2)*exp(THETA(3)*COMED_VPA+THETA(4)*COMED_CBZ+THETA(5)*COMED_PHB+ETA(1))",
        "V=THETA(6)*(WT/27.87)", "KA=THETA(7)", "S2=V"
      ),
      "Y=F*(1+ERR(1))"
    ),
    covariates = "WT", optional = c("COMED_VPA", "COMED_CBZ", "COMED_PHB"),
    reference_id = "aed_lmt_23103620_m01", pmid = "23103620",
    corrections = paste(
      "Corrected the legacy AEDapt volume implementation to the published",
      "V/F = 16.7 * (WT / 27.87) L relationship."
    ),
    title = "Lamotrigine population PK - He et al.", advan = 2, trans = 2,
    version = "1.0.1"
  ),
  spec(
    "aedapt_lorazepam_gonzalez", "LRZP_AP_GONZALEZ.R", "lorazepam", "Gonzalez",
    c(.3, 18), "Paediatric multinational population", "mcg/L",
    control(
      "Lorazepam - Gonzalez population PK", 4, 4, c("WT", "AGE"),
      c(
        th(.879, "V1/F per kg"), th(.115, "CL/F coefficient"),
        th(.542, "V2/V1 ratio"), th(1.45, "Q/F"),
        th(.133, "age exponent"), th(1, "KA")
      ),
      diag_omega(.20, .15, .28), sigma(.1),
      lines(
        "V1=THETA(1)*WT*exp(ETA(1))",
        "CL=THETA(2)*(WT^0.75)*(AGE/4.7)^THETA(5)*exp(ETA(2))",
        "V2=THETA(3)*THETA(1)*WT", "Q=THETA(4)",
        "KA=THETA(6)*exp(ETA(3))", "S2=V1", "S3=V2"
      ),
      "Y=F*(1+ERR(1))"
    ),
    covariates = c("WT", "AGE"),
    title = "Lorazepam population PK - Gonzalez legacy model", advan = 4, trans = 4
  ),
  spec(
    "aedapt_levetiracetam_toublanc", "LVT_AP_TOUBLANC.R", "levetiracetam", "Toublanc",
    c(4.3, 55.4), "Japanese and North American people with epilepsy", "mg/L",
    control(
      "Levetiracetam - Toublanc population PK", 2, 2,
      c("WT", "FORMULATION_IND"),
      c(
        th(2.56, "KA"), th(2.10, "CL/F at 32 kg"),
        th(1.22, "formulation factor"), th(20.4, "V/F at 32 kg")
      ),
      block_omega(3, c(.736, 0, .0396, 0, .00328, .0149)),
      sigma(.0357),
      lines(
        "KA=THETA(1)*exp(ETA(1))",
        "CL=THETA(2)*(THETA(3)^FORMULATION_IND)*(WT/32)^0.75*exp(ETA(2))",
        "V=THETA(4)*(WT/32)*exp(ETA(3))", "S2=V"
      ),
      "Y=F*(1+ERR(1))"
    ),
    covariates = "WT", optional = "FORMULATION_IND",
    reference_id = "aed_lvt_23877106_m01", pmid = "23877106",
    corrections = "Rebuilt the CL/V covariance as the reported 0.00328 NONMEM covariance; AEDapt square-rooted matrix elements before covariance simulation.",
    title = "Levetiracetam population PK - Toublanc et al.", advan = 2, trans = 2
  ),
  spec(
    "aedapt_oxcarbazepine_park", "OXC_AP_PARK.R", "oxcarbazepine", "Park",
    c(18, 80), "Adult people with epilepsy", "mg/L",
    control(
      "Oxcarbazepine active-metabolite population PK - Park", 2, 2,
      c("WT", "EIAED"),
      c(
        th(2.13, "CL/F"), th(.666, "weight exponent"),
        th(.598, "KA"), th(49, "V/F"), th(.312, "enzyme-inducer effect")
      ),
      diag_omega(.0767), sigma(.0571, 2.83),
      lines(
        "CL=THETA(1)*(WT/62.8)^THETA(2)*(1+THETA(5)*EIAED)*exp(ETA(1))",
        "KA=THETA(3)", "V=THETA(4)*(WT/62.8)", "S2=V"
      ),
      "Y=F*(1+ERR(1))+ERR(2)"
    ),
    covariates = "WT", optional = "EIAED",
    reference_id = "aed_oxc_22246398_m01", pmid = "22246398",
    title = "Oxcarbazepine active-metabolite population PK - Park et al.",
    advan = 2, trans = 2
  ),
  spec(
    "aedapt_phenobarbital_goto", "PHB_AP_GOTO.R", "phenobarbital", "Goto",
    c(.1, 19.9), "Japanese paediatric people with epilepsy", "mg/L",
    control(
      "Phenobarbital - Goto population PK", 2, 2,
      c("WT", "CYP2C9_13", "COMED_VPA", "COMED_PHT", "SEVERE_MID"),
      c(
        th(14.78, "V/F"), th(.23, "CL/F at 40 kg"),
        th(.21, "weight exponent"), th(.53, "CYP2C9*1/*3 factor"),
        th(.68, "valproate factor"), th(.85, "phenytoin factor"),
        th(.85, "severe mental/intellectual disability factor"),
        th(2, "KA", fixed = TRUE)
      ),
      diag_omega(2.93, .03), sigma(12.2),
      lines(
        "V=THETA(1)*(1+ETA(1))",
        "CL=THETA(2)*(WT/40)^THETA(3)*(THETA(4)^CYP2C9_13)*(THETA(5)^COMED_VPA)*(THETA(6)^COMED_PHT)*(THETA(7)^SEVERE_MID)*(1+ETA(2))",
        "KA=THETA(8)", "S2=V"
      ),
      "Y=F+ERR(1)"
    ),
    covariates = c("WT", "CYP2C9_13", "SEVERE_MID"),
    optional = c("COMED_VPA", "COMED_PHT"),
    reference_id = "aed_phb_17304159_m01", pmid = "17304159",
    corrections = "Restored CYP2C9 and severe-disability covariate effects omitted by AEDapt.",
    assumptions = "KA=2 h^-1 was retained as a fixed generated default because it was not reported.",
    title = "Phenobarbital population PK - Goto et al.", advan = 2, trans = 2
  ),
  spec(
    "aedapt_phenytoin_odani", "PHT_AP_ODANI.R", "phenytoin", "Odani",
    c(4, 79), "Japanese people with epilepsy", "mg/L",
    control(
      "Phenytoin - Odani nonlinear population PK", 13, NULL,
      c("WT", "COMED_ZNS"),
      c(
        th(9.80, "VM per size per day"), th(9.19, "KM"),
        th(1.23, "V per size"), th(.463, "size exponent"),
        th(1.16, "zonisamide factor"), th(10, "KA", fixed = TRUE)
      ),
      diag_omega(.023, .098, .206), sigma(.033),
      lines(
        "SIZE=42*(WT/42)^THETA(4)",
        "VM=THETA(1)*SIZE*exp(ETA(1))",
        "KM=THETA(2)*(THETA(5)^COMED_ZNS)*exp(ETA(2))",
        "V=THETA(3)*SIZE*exp(ETA(3))", "KA=THETA(6)", "S2=V"
      ),
      "Y=F*(1+ERR(1))",
      model = c("COMP=(DEPOT,DEFDOSE)", "COMP=(CENTRAL,DEFOBSERVATION)"),
      des = c(
        "DADT(1)=-KA*A(1)",
        "C=A(2)/V",
        "DADT(2)=KA*A(1)-(VM/24)*C/(KM+C)"
      )
    ),
    covariates = "WT", optional = "COMED_ZNS",
    reference_id = "aed_pht_8924916_m01", pmid = "8924916",
    assumptions = "KA=10 h^-1 was retained as a fixed AEDapt assumption; it was not reported.",
    corrections = "Implemented the reported Michaelis-Menten elimination dynamically rather than treating dose-dependent steady-state clearance as constant.",
    title = "Phenytoin nonlinear population PK - Odani et al.",
    model_type = "nonlinear_pk", advan = 13, trans = 1, use_ode = TRUE
  ),
  spec(
    "aedapt_phenytoin_yukawa", "PHT_AP_YUKAWA.R", "phenytoin", "Yukawa",
    c(.2, 15), "Japanese paediatric people with epilepsy", "mg/L",
    control(
      "Phenytoin - Yukawa nonlinear population PK", 13, NULL,
      c("WT", "AGE", "COMED_AED", "POWDER_FORMULATION", "DAILY_DOSE"),
      c(
        th(325, "VM at 60 kg per day"), th(2.41, "KM"),
        th(.737, "weight exponent"), th(.752, "age over 15 factor"),
        th(1.08, "comedication VM factor"), th(1.32, "comedication KM factor"),
        th(9.92, "powder bioavailability coefficient"),
        th(1.23, "V per kg", fixed = TRUE), th(10, "KA", fixed = TRUE)
      ),
      diag_omega(.0366, .335, .206), sigma(.0106),
      lines(
        "AGE15=ifelse(AGE>15,1,0)",
        "KM=THETA(2)*(THETA(4)^AGE15)*(THETA(6)^COMED_AED)*exp(ETA(1))",
        "VM=THETA(1)*(WT/60)^THETA(3)*(THETA(5)^COMED_AED)*exp(ETA(2))",
        "V=THETA(8)*WT*exp(ETA(3))", "KA=THETA(9)",
        "F1=1-POWDER_FORMULATION*exp(-THETA(7)/DAILY_DOSE)", "S2=V"
      ),
      "Y=F*(1+ERR(1))",
      model = c("COMP=(DEPOT,DEFDOSE)", "COMP=(CENTRAL,DEFOBSERVATION)"),
      des = c(
        "DADT(1)=-KA*A(1)",
        "C=A(2)/V",
        "DADT(2)=KA*A(1)-(VM/24)*C/(KM+C)"
      )
    ),
    covariates = c("WT", "AGE", "DAILY_DOSE"),
    optional = c("COMED_AED", "POWDER_FORMULATION"),
    reference_id = "aed_pht_2268899_m01", pmid = "2268899",
    assumptions = c(
      "V=1.23*WT and its variance were borrowed by AEDapt from Odani et al.",
      "KA=10 h^-1 was an AEDapt generated default; both are fixed except the borrowed V variability."
    ),
    corrections = "Implemented the steady-state Michaelis-Menten relationship as a dynamic ODE and retained the reported dose-dependent powder formulation bioavailability.",
    title = "Phenytoin nonlinear population PK - Yukawa et al.",
    model_type = "nonlinear_pk", advan = 13, trans = 1, use_ode = TRUE
  ),
  spec(
    "aedapt_topiramate_jovanovic", "TPM_A_JOVANOVIC.R", "topiramate", "Jovanovic",
    c(18, 80), "Adult people with epilepsy", "mg/L",
    control(
      "Topiramate - Jovanovic population PK", 2, 2,
      c("WT", "MDRD", "CBZ_DAILY_DOSE"),
      c(
        th(2, "KA"), th(1.53, "base CL/F"), th(.575, "V/F per kg"),
        th(.476, "carbamazepine-dose effect"), th(.00476, "MDRD effect")
      ),
      diag_omega(.027), sigma(.134),
      lines(
        "CL=THETA(2)*(1+THETA(4)*CBZ_DAILY_DOSE/1000)*exp(THETA(5)*(MDRD-95.72)+ETA(1))",
        "V=THETA(3)*WT", "KA=THETA(1)", "S2=V"
      ),
      "Y=F*(1+ERR(1))"
    ),
    covariates = c("WT", "MDRD", "CBZ_DAILY_DOSE"),
    reference_id = "aed_tpm_23891703_m01", pmid = "23891703",
    corrections = "Removed AEDapt's randomly simulated MDRD/sex values; measured MDRD and carbamazepine daily dose are required.",
    title = "Topiramate population PK - Jovanovic et al.", advan = 2, trans = 2
  ),
  spec(
    "aedapt_topiramate_girgis", "TPM_AP_GIRGIS.R", "topiramate", "Girgis",
    c(2, 85), "People with epilepsy", "mg/L",
    control(
      "Topiramate - Girgis population PK", 4, 4,
      c("WT", "AGE", "ADJUNCTIVE", "INDUCER", "COMED_VPA", "NONINDUCER"),
      c(
        th(1.21, "base CL/F"), th(.479, "adjunctive effect"),
        th(.453, "CL weight exponent"), th(-.00306, "CL age coefficient"),
        th(1.94, "inducer factor"), th(.686, "valproate factor"),
        th(.635, "non-inducer factor"), th(4.61, "V1/F"),
        th(1.14, "V1 weight exponent"), th(.105, "KA"),
        th(.577, "K12"), th(.0586, "K21")
      ),
      diag_omega(.0744, 1.35, .0499), sigma(.06482116, .03229209),
      lines(
        "CL=THETA(1)*(1+ADJUNCTIVE*THETA(2))*(WT/69.9)^THETA(3)*exp(THETA(4)*(AGE-31.4))*(THETA(5)^INDUCER)*(THETA(6)^COMED_VPA)*(THETA(7)^NONINDUCER)*exp(ETA(1))",
        "V1=THETA(8)*(WT/69.9)^THETA(9)*exp(ETA(2))",
        "Q=THETA(11)*V1", "V2=V1*THETA(11)/THETA(12)",
        "KA=THETA(10)*exp(ETA(3))", "S2=V1", "S3=V2"
      ),
      "Y=F*(1+ERR(1))+ERR(2)"
    ),
    covariates = c("WT", "AGE"),
    optional = c("ADJUNCTIVE", "INDUCER", "COMED_VPA", "NONINDUCER"),
    reference_id = "aed_tpm_20880232_m01", pmid = "20880232",
    corrections = "Converted reported K12/K21 values to Q and V2 for ADVAN4/TRANS4.",
    title = "Topiramate population PK - Girgis et al.", advan = 4, trans = 4
  ),
  spec(
    "aedapt_valproate_blanco_adult", "VPA_A_BLANCO.R", "valproate", "Blanco",
    c(16, 86), "Adult people with epilepsy", "mg/L",
    control(
      "Valproate - Blanco adult population PK", 2, 2,
      c("WT", "DOSE_MG_KG_DAY", "COMED_CBZ", "COMED_PHT", "COMED_PHB"),
      c(
        th(.004, "base CL/F per kg"), th(.304, "dose exponent"),
        th(1.363, "carbamazepine factor"), th(1.541, "phenytoin factor"),
        th(1.397, "phenobarbital factor"), th(1.2, "KA"),
        th(.2, "V/F per kg")
      ),
      diag_omega(.0547), sigma(11.36),
      lines(
        "CL=THETA(1)*WT*(DOSE_MG_KG_DAY^THETA(2))*(THETA(3)^COMED_CBZ)*(THETA(4)^COMED_PHT)*(THETA(5)^COMED_PHB)*(1+ETA(1))",
        "KA=THETA(6)", "V=THETA(7)*WT", "S2=V"
      ),
      "Y=F+ERR(1)"
    ),
    covariates = c("WT", "DOSE_MG_KG_DAY"),
    optional = c("COMED_CBZ", "COMED_PHT", "COMED_PHB"),
    reference_id = "aed_vpa_10594867_m01", pmid = "10594867",
    corrections = "Restored the reported multiplicative comedication factors; AEDapt had implemented them as 1+effect.",
    title = "Valproate adult population PK - Blanco et al.", advan = 2, trans = 2
  ),
  spec(
    "aedapt_valproate_blanco_paediatric", "VPA_P_BLANCO.R", "valproate", "Blanco",
    c(.2, 15), "Paediatric people with epilepsy", "mg/L",
    control(
      "Valproate - Blanco paediatric population PK", 2, 2,
      c("WT", "DOSE_MG_KG_DAY", "COMED_CBZ"),
      c(
        th(.012, "base CL/F"), th(.715, "weight exponent"),
        th(.306, "dose exponent"), th(1.359, "carbamazepine factor"),
        th(1.9, "KA"), th(.24, "V/F per kg")
      ),
      diag_omega(.046255), sigma(.1556),
      lines(
        "CL=THETA(1)*(WT^THETA(2))*(DOSE_MG_KG_DAY^THETA(3))*(THETA(4)^COMED_CBZ)*exp(ETA(1))",
        "KA=THETA(5)", "V=THETA(6)*WT", "S2=V"
      ),
      "Y=F*(1+ERR(1))"
    ),
    covariates = c("WT", "DOSE_MG_KG_DAY"), optional = "COMED_CBZ",
    reference_id = "aed_vpa_10319910_m01", pmid = "10319910",
    corrections = "Restored the reported carbamazepine multiplicative factor (1.359^indicator).",
    title = "Valproate paediatric population PK - Blanco et al.", advan = 2, trans = 2
  ),
  spec(
    "aedapt_zonisamide_hashimoto", "ZNS_AP_HASHIMOTO.R", "zonisamide", "Hashimoto",
    c(4, 79), "Japanese people with epilepsy", "mg/L",
    control(
      "Zonisamide - Hashimoto nonlinear population PK", 13, NULL,
      c("WT", "COMED_CBZ"),
      c(
        th(.741, "size exponent"), th(1.13, "carbamazepine VM factor"),
        th(1.27, "V per size"), th(27.6, "VM per size per day"),
        th(45.9, "KM"), th(2, "KA", fixed = TRUE)
      ),
      diag_omega(.0882), sigma(.0317),
      lines(
        "SIZE=33*(WT/33)^THETA(1)",
        "V=THETA(3)*SIZE", "VM=THETA(4)*SIZE*(THETA(2)^COMED_CBZ)",
        "KM=THETA(5)*(1+ETA(1))", "KA=THETA(6)", "S2=V"
      ),
      "Y=F*(1+ERR(1))",
      model = c("COMP=(DEPOT,DEFDOSE)", "COMP=(CENTRAL,DEFOBSERVATION)"),
      des = c(
        "DADT(1)=-KA*A(1)",
        "C=A(2)/V",
        "DADT(2)=KA*A(1)-(VM/24)*C/(KM+C)"
      )
    ),
    covariates = "WT", optional = "COMED_CBZ",
    reference_id = "aed_zns_8205132_m01", pmid = "8205132",
    assumptions = "KA=2 h^-1 was added as a fixed generated default for dynamic oral dosing.",
    corrections = "Restored the reported normal ETA on KM and implemented Michaelis-Menten elimination dynamically; AEDapt approximated a dose-dependent linear clearance.",
    title = "Zonisamide nonlinear population PK - Hashimoto et al.",
    model_type = "nonlinear_pk", advan = 13, trans = 1, use_ode = TRUE
  ),
  spec(
    "aedapt_zonisamide_okada", "ZNS_AP_OKADA.R", "zonisamide", "Okada",
    c(.2, 15), "Japanese paediatric people with epilepsy", "mg/L",
    control(
      "Zonisamide - Okada population PK", 2, 2,
      c("WT", "DAILY_DOSE", "CYP2C19_HET", "CYP2C19_PM",
        "COMED_CBZ", "COMED_PHT", "COMED_PHB"),
      c(
        th(1.22, "base CL/F"), th(.77, "weight exponent"),
        th(-.17, "daily-dose exponent"), th(.84, "CYP2C19 heterozygote factor"),
        th(.70, "CYP2C19 poor-metaboliser factor"),
        th(1.24, "carbamazepine factor"), th(1.28, "phenytoin factor"),
        th(1.29, "phenobarbital factor"),
        th(1.23, "V/F per kg", fixed = TRUE), th(2, "KA", fixed = TRUE)
      ),
      diag_omega(.076), sigma(.05),
      lines(
        "CL=THETA(1)*(WT/44)^THETA(2)*(DAILY_DOSE^THETA(3))*(THETA(4)^CYP2C19_HET)*(THETA(5)^CYP2C19_PM)*(THETA(6)^COMED_CBZ)*(THETA(7)^COMED_PHT)*(THETA(8)^COMED_PHB)*exp(ETA(1))",
        "V=THETA(9)*WT", "KA=THETA(10)", "S2=V"
      ),
      "Y=F*(1+ERR(1))"
    ),
    covariates = c("WT", "DAILY_DOSE", "CYP2C19_HET", "CYP2C19_PM"),
    optional = c("COMED_CBZ", "COMED_PHT", "COMED_PHB"),
    reference_id = "aed_zns_18641551_m01", pmid = "18641551",
    assumptions = c(
      "V/F=1.23*WT was borrowed by AEDapt from an Odani phenytoin model.",
      "KA=2 h^-1 was an AEDapt generated default; both structural values are fixed."
    ),
    title = "Zonisamide population PK - Okada et al.", advan = 2, trans = 2
  )
)

read_reference <- function(id) {
  if (!nzchar(id)) return(NULL)
  path <- file.path(reference_root, paste0(id, ".json"))
  if (!file.exists(path)) stop("Mapped reference record is missing: ", path)
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

candidate_record <- function(item, model_sha) {
  population <- list()
  if (length(item$age) == 2L) {
    population$age <- list(
      min = item$age[[1L]], max = item$age[[2L]], unit = "years",
      required = TRUE, hard = TRUE, covariate = "AGE"
    )
  }
  list(
    schema = "liberary.clinical_qualification",
    schema_version = "1.0.0",
    qualification_id = paste0("cq-", item$id, "-candidate"),
    version = item$version,
    status = "candidate",
    scope = list(
      drugs = item$drug, analytes = item$drug, metabolites = list(),
      indications = c("epilepsy", "seizure disorder"),
      routes = "oral", formulations = list(), regimens = list(),
      endpoint_ids = list(), endpoint_kinds = "therapeutic_range",
      population = population,
      covariates = list(
        required = item$covariates, optional = item$optional,
        supported_imputation = list(), ranges = list()
      ),
      assays = list(
        required = TRUE, matrices = c("plasma", "serum"), methods = list(),
        units = item$units
      )
    ),
    evidence = list(
      source = "AEDapt legacy implementation plus AED PK/PD review corpus",
      computational_translation = "control-stream parse checked",
      clinical_validation = "not performed",
      model_sha256 = model_sha
    ),
    governance = list(
      issuer = "LibeR AEDapt migration",
      reviewer = "",
      valid_from = "2026-07-29T00:00:00Z",
      review_due = ""
    ),
    limitations = unique(c(
      "Research candidate only; independent source verification and clinical qualification are required before patient-care use.",
      item$assumptions
    )),
    out_of_domain_rules = list(
      reject_outside_age_range = TRUE,
      reject_missing_required_covariates = TRUE,
      require_qualified_status_for_automatic_selection = TRUE
    ),
    supersedes = "",
    notes = "Migrated from AEDapt; catalogue availability is not clinical endorsement.",
    created_at = "2026-07-29T00:00:00Z",
    model = list(
      library_id = item$id, version = item$version,
      model_sha256 = model_sha, manifest_schema_version = "1.5.0"
    )
  )
}

write_entry <- function(item) {
  source_path <- file.path(aedapt_library, item$source)
  if (!file.exists(source_path)) stop("AEDapt source model is missing: ", source_path)
  reference <- read_reference(item$reference_id)
  extraction <- reference$reference$extraction_target %||% list()
  title <- item$title
  if (!nzchar(title)) title <- extraction$title %||% paste(item$drug, item$author)
  ctl_text <- paste(item$ctl, collapse = "\n")
  model_sha <- digest::digest(charToRaw(ctl_text), algo = "sha256", serialize = FALSE)
  source_sha <- digest::digest(file = source_path, algo = "sha256", serialize = FALSE)
  entry <- file.path(entries_root, item$id)
  provenance_dir <- file.path(entry, "extraction")
  dir.create(provenance_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(item$ctl), file.path(entry, "model.ctl"), useBytes = TRUE)

  study_population <- extraction$population %||% item$population
  n_subjects <- extraction$n_subjects %||% NULL
  manifest <- list(
    schema_version = "1.5.0",
    library_id = item$id,
    version = item$version,
    status = "review",
    title = title,
    model = list(
      artifact = "model.ctl", advan = item$advan, trans = item$trans,
      type = item$model_type, use_ode = item$use_ode,
      generated_suggestion = TRUE, mapping_review_required = TRUE
    ),
    study = list(
      compound = item$drug, population = study_population,
      population_details = extraction$population_details %||% list(),
      route = "oral", n_subjects = n_subjects,
      assay_unit = item$units,
      keywords = c(
        "population pk", "antiseizure medicine", "AEDapt migration",
        if (item$use_ode) "nonlinear elimination" else "compartmental"
      ),
      dataset_fingerprint = if (nzchar(item$pmid)) paste0("pmid:", item$pmid) else
        paste0("aedapt:", source_sha)
    ),
    confidence = list(
      overall = if (length(item$assumptions)) .72 else .86,
      fields = list(
        structure = if (length(item$assumptions)) .70 else .90,
        parameters = .90, population = if (is.null(reference)) .65 else .95
      )
    ),
    provenance = list(
      source_type = "legacy_curated_model",
      origin = "AEDapt",
      source_file = item$source,
      source_sha256 = source_sha,
      source_repository_included = FALSE,
      reference_corpus_id = item$reference_id,
      pmid = item$pmid,
      migration_method = "deterministic curated translation",
      corrections = item$corrections,
      assumptions = item$assumptions,
      description = paste(
        "Translated from the legacy AEDapt model definition.",
        "Published information was preferred over executable legacy code when they differed."
      )
    ),
    qualification = list(
      author_validated = FALSE,
      curator = "LibeR AEDapt migration - independent review pending",
      clinical_use = list(candidate_record(item, model_sha))
    ),
    relations = list(
      same_compound = list(), prior_version = list(), mbma_pool = FALSE
    ),
    created_at = "2026-07-29T00:00:00Z",
    updated_at = "2026-07-29T00:00:00Z"
  )
  jsonlite::write_json(
    manifest, file.path(entry, "manifest.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA
  )
  jsonlite::write_json(
    list(
      schema = "liberary.aedapt_migration",
      schema_version = "1.0.0",
      library_id = item$id,
      source = list(
        application = "AEDapt", file = item$source, sha256 = source_sha,
        author_label = item$author
      ),
      reference = list(
        corpus_id = "aed-pkpd-reference",
        corpus_version = "0.2.2",
        reference_id = item$reference_id,
        pmid = item$pmid
      ),
      decisions = list(
        corrections = item$corrections,
        assumptions = item$assumptions,
        clinical_status = "candidate"
      )
    ),
    file.path(provenance_dir, "aedapt-migration.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA
  )
  citation <- if (nzchar(item$pmid)) {
    c(
      paste0("@article{pmid", item$pmid, ","),
      paste0("  title = {", gsub("[{}]", "", title), "},"),
      paste0("  note = {PubMed PMID ", item$pmid, "},"),
      paste0("  url = {https://pubmed.ncbi.nlm.nih.gov/", item$pmid, "/}"),
      "}"
    )
  } else {
    c(
      paste0("@misc{", item$id, ","),
      paste0("  title = {", gsub("[{}]", "", title), "},"),
      paste0("  author = {", item$author, "},"),
      "  note = {Legacy AEDapt attribution; primary publication identifier requires review}",
      "}"
    )
  }
  writeLines(citation, file.path(entry, "references.bib"), useBytes = TRUE)
  invisible(manifest)
}

manifests <- lapply(specs, write_entry)
existing <- list.dirs(entries_root, recursive = FALSE, full.names = TRUE)
records <- lapply(existing, function(directory) {
  path <- file.path(directory, "manifest.json")
  if (!file.exists(path)) return(NULL)
  value <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  list(
    library_id = value$library_id,
    title = value$title,
    status = value$status,
    updated_at = value$updated_at %||% value$created_at %||% ""
  )
})
records <- Filter(Negate(is.null), records)
records <- records[order(vapply(records, `[[`, character(1), "library_id"))]
jsonlite::write_json(
  list(
    schema_version = "1.5.0",
    updated_at = "2026-07-29T00:00:00Z",
    entries = records
  ),
  file.path(catalog_root, "index.json"),
  auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA
)

message("Imported ", length(specs), " AEDapt antiseizure-medicine models.")
