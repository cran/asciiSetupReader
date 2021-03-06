context("test-parser-missing-sps")

test_that("Right number of missing values", {

  expect_equal(nrow(sac_parsed_sps$missing), 261)
  expect_equal(nrow(sex_offender_parsed_sps$missing), 18)
  expect_true(is.null(ucr1960_parsed_sps$missing))
  expect_equal(nrow(weimar_parsed_sps$missing), 19)
  expect_true(is.null(acs_parsed_sps$missing))
  expect_true(is.null(nibrs_parsed_sps$missing))
  expect_equal(nrow(parole_parsed_sps$missing), 89)
  expect_equal(nrow(prisoners_parsed_sps$missing), 1800)
  expect_true(is.null(ca_vital_parsed_sps$missing))
  expect_equal(nrow(crosswalk_parsed_sps$missing), 12)
  expect_equal(nrow(ucr1985_parsed_sps$missing), 166)
  expect_equal(nrow(ucr1986_parsed_sps$missing), 160)
  expect_true(is.null(ucr2000_parsed_sps$missing))
  expect_equal(nrow(ncvs_parsed_sps$missing), 80)
  expect_true(is.null(jail_1987_parsed_sps$missing))
  expect_equal(nrow(jail_2010_parsed_sps$missing), 60)
  expect_equal(nrow(corrections_parsed_sps$missing), 7)
  expect_equal(nrow(sadc_parsed_sps$missing), 312)
  expect_true(is.null(well_being_parsed_sps$missing))
  #expect_true(is.null(escolar_parsed_sps$missing))
  expect_true(is.null(health_nutrition_parsed_sps$missing))
  expect_true(is.null(ad_health_parsed_sps$missing))
  expect_equal(nrow(india_human_parsed_sps$missing), 1)
  expect_equal(nrow(census_police_parsed_sps$missing), 74)
  expect_equal(nrow(step_in_parsed_sps$missing), 2)
  expect_equal(nrow(cps_1973_parsed_sps$missing), 235)
  expect_true(is.null(cps_2004_parsed_sps$missing))
  expect_equal(nrow(drug_abuse_parsed_sps$missing), 424)
  expect_equal(nrow(british_crime_teen_parsed_sps$missing), 1119)
  expect_true(is.null(detroit_parsed_sps$missing))
  expect_true(is.null(worry_parsed_sps$missing))

  expect_equal(nrow(cambridge_parsed_sps$missing), 1068)
  expect_true(is.null(guam_parsed_sps$missing))
  expect_true(is.null(china_2002_parsed_sps$missing))
  expect_true(is.null(china_1995_parsed_sps$missing))
  expect_equal(nrow(china_1998_parsed_sps$missing), 47)
  expect_true(is.null(indonesia_parsed_sps$missing))
  expect_equal(nrow(UN_crime_parsed_sps$missing), 357)
  expect_equal(nrow(county_arrest_parsed_sps$missing), 13)
  expect_true(is.null(escolar_2006_parsed_sps$missing))


  expect_true(is.null(mtf_1999_parsed_sps$missing))
  expect_equal(nrow(mtf_2003_parsed_sps$missing), 108)
  expect_equal(nrow(mtf_1990_parsed_sps$missing), 206)
  expect_equal(nrow(mtf_1989_parsed_sps$missing), 224)
  expect_equal(nrow(mtf_2004_parsed_sps$missing), 230)
  expect_equal(nrow(mtf_2002_parsed_sps$missing), 108)
  expect_equal(nrow(mtf_1993_parsed_sps$missing), 206)
  expect_equal(nrow(mtf_1991_parsed_sps$missing), 206)
  expect_equal(nrow(mtf_1992_parsed_sps$missing), 206)
  expect_equal(nrow(mtf_1979_parsed_sps$missing), 544)
})

test_that("CA SEDD 2005 has right missing values", {
  expect_equal(ca_sedd_2005_ahal_parsed_sps$missing$variable,
               c("HOSPSTCO", "HOSPSTCO", "HOSPSTCO", "HOSPSTCO"))
  expect_equal(ca_sedd_2005_ahal_parsed_sps$missing$values,
               c("-9999", "-8888", "-6666", "-5555"))

  expect_equal(unique(ca_sedd_2005_ahal_parsed_sps$missing$variable),
               "HOSPSTCO")

})

test_that("missing_value_no_s_parsed_sps has right missing values", {
  expect_equal(head(missing_value_no_s_parsed_sps$missing$variable),
               c("V10", "V12R1", "V12R1",
                 "V12R2", "V12R2", "V12R3"))
  expect_equal(head(missing_value_no_s_parsed_sps$missing$values),
               c("999 THRU HIGHEST", "0", "96 THRU HIGHEST",
                 "0", "96 THRU HIGHEST", "0"))

  expect_equal(tail(missing_value_no_s_parsed_sps$missing$variable),
               c("V4018", "V4019", "V4020",
                 "V4021", "V4022", "V4023"))
  expect_equal(tail(missing_value_no_s_parsed_sps$missing$values),
               c("99 THRU HIGHEST", "99 THRU HIGHEST", "0",
                 "0", "0", "0"))

  expect_equal(head(unique(missing_value_no_s_parsed_sps$missing$variable)),
               c("V10", "V12R1", "V12R2",
                 "V12R3", "V13R1", "V13R2"))
  expect_equal(tail(unique(missing_value_no_s_parsed_sps$missing$variable)),
               c("V4018", "V4019", "V4020",
                 "V4021", "V4022", "V4023"))
})


test_that("Cambridge has right missing values", {
  expect_equal(head(cambridge_parsed_sps$missing$variable),
               c("V7", "V8", "V10",
                 "V11", "V12", "V12"))
  expect_equal(head(cambridge_parsed_sps$missing$values),
               c("0", "0", "0",
                 "0", "0", "9"))

  expect_equal(tail(cambridge_parsed_sps$missing$variable),
               c("V876", "V877", "V877",
                 "V878", "V879", "V880"))
  expect_equal(tail(cambridge_parsed_sps$missing$values),
               c("98", "0", "98",
                 "0", "0", "0"))
  expect_equal(cambridge_parsed_sps$missing$values[cambridge_parsed_sps$missing$variable == "V747"],
               c("6", "8", "9"))

  expect_equal(head(unique(cambridge_parsed_sps$missing$variable)),
               c("V7", "V8", "V10",
                 "V11", "V12", "V20"))
  expect_equal(tail(unique(cambridge_parsed_sps$missing$variable)),
               c("V875", "V876", "V877",
                 "V878", "V879", "V880"))
})

test_that("China 1998 has right missing values", {
  expect_equal(head(china_1998_parsed_sps$missing$variable),
               c("RELATION", "GENDER", "AGE",
                 "STUDENT", "INCOME88", "RESIDENC"))
  expect_equal(head(china_1998_parsed_sps$missing$values),
               c("9", "9", "999",
                 "9", "9", "9"))

  expect_equal(tail(china_1998_parsed_sps$missing$variable),
               c("IT07T", "IT07M", "IT07E",
                 "IT08T", "IT08M", "IT08E"))
  expect_equal(tail(china_1998_parsed_sps$missing$values),
               c("9", "99999", "99999",
                 "9", "99999", "99999"))

  expect_equal(head(unique(china_1998_parsed_sps$missing$variable)),
               c("RELATION", "GENDER", "AGE",
                 "STUDENT", "INCOME88", "RESIDENC"))
  expect_equal(tail(unique(china_1998_parsed_sps$missing$variable)),
               c("IT07T", "IT07M", "IT07E",
                 "IT08T", "IT08M", "IT08E"))
})

test_that("UN Crime has right missing values", {
  expect_equal(head(UN_crime_parsed_sps$missing$variable),
               c("NNHOM70N", "NNHOM70N", "NNHOM70N",
                 "NNHOM71N", "NNHOM71N", "NNHOM71N"))
  expect_equal(head(UN_crime_parsed_sps$missing$values),
               c("-2", "-3", "-9",
                 "-2", "-3", "-9"))

  expect_equal(tail(UN_crime_parsed_sps$missing$variable),
               c("X5", "X5", "X5",
                 "X6", "X6", "X6"))
  expect_equal(tail(UN_crime_parsed_sps$missing$values),
               c("-2", "-3", "-9",
                 "-2", "-3", "-9"))

  expect_equal(head(unique(UN_crime_parsed_sps$missing$variable)),
               c("NNHOM70N", "NNHOM71N", "NNHOM72N",
                 "NNHOM73N", "NNHOM74N", "NNHOM75N"))
  expect_equal(tail(unique(UN_crime_parsed_sps$missing$variable)),
               c("PSTF745", "X2", "X3",
                 "X4", "X5", "X6"))
})

test_that("County arrest has right missing values", {
  expect_equal(head(county_arrest_parsed_sps$missing$variable),
               c("V7", "V8", "V9",
                 "V10", "V11", "V12"))
  expect_equal(head(county_arrest_parsed_sps$missing$values),
               c("9999999", "9999999", "999999",
                 "99999", "99999", "9999"))

  expect_equal(tail(county_arrest_parsed_sps$missing$variable),
               c("V14", "V15", "V16",
                 "V17", "V18", "V19"))
  expect_equal(tail(county_arrest_parsed_sps$missing$values),
               c("99999", "99999", "99999",
                 "99999", "99999", "9999"))

  expect_equal(head(unique(county_arrest_parsed_sps$missing$variable)),
               c("V7", "V8", "V9",
                 "V10", "V11", "V12"))
  expect_equal(tail(unique(county_arrest_parsed_sps$missing$variable)),
               c("V14", "V15", "V16",
                 "V17", "V18", "V19"))
})

test_that("British Crime Teen has right missing values", {
  expect_equal(head(british_crime_teen_parsed_sps$missing$variable),
               c("TB_CASE", "TB_CASE", "TB_CASE",
                 "AR_CODE", "AR_CODE", "AR_CODE"))
  expect_equal(tail(british_crime_teen_parsed_sps$missing$variable),
               c("T73", "T73", "T73",
                 "T74", "T74", "T74"))

  expect_equal(head(british_crime_teen_parsed_sps$missing$values),
               c("-7", "-8", "-9",
                 "-7", "-8", "-9"))
  expect_equal(tail(british_crime_teen_parsed_sps$missing$values),
               c("-7", "-8", "-9",
                 "-7", "-8", "-9"))

  expect_equal(head(unique(british_crime_teen_parsed_sps$missing$variable)),
               c("TB_CASE", "AR_CODE", "T_SN",
                 "T_SCRN", "BOOSTER", "CARD_28"))
  expect_equal(tail(unique(british_crime_teen_parsed_sps$missing$variable)),
               c("T69", "T70", "T71",
                 "T72", "T73", "T74"))
})

test_that("Drug Abuse has right missing values", {
  expect_equal(head(drug_abuse_parsed_sps$missing$variable),
               c("ID", "RESPCODE", "SITEID",
                 "DATE", "DEGREE", "YEAR_DEG"))
  expect_equal(tail(drug_abuse_parsed_sps$missing$variable),
               c("DOCLEAD", "EOTDIV", "EOTTOL",
                 "EOTSCO", "EOTOPN", "EOTOPN"))

  expect_equal(head(drug_abuse_parsed_sps$missing$values),
               c("-9", "-9", "-9",
                 "-9", "-9", "-9"))
  expect_equal(tail(drug_abuse_parsed_sps$missing$values),
               c("-9.0", "-9.0", "-9.0",
                 "-9", "-9.0", "-5.0"))

  expect_equal(head(unique(drug_abuse_parsed_sps$missing$variable)),
               c("ID", "RESPCODE", "SITEID",
                 "DATE", "DEGREE", "YEAR_DEG"))
  expect_equal(tail(unique(drug_abuse_parsed_sps$missing$variable)),
               c("DOCSUP", "DOCLEAD", "EOTDIV",
                 "EOTTOL", "EOTSCO", "EOTOPN"))
})


test_that("Step In has right missing values", {
  expect_equal(step_in_parsed_sps$missing$variable,
               c("NR_DAYS", "CHARGE"))
  expect_equal(step_in_parsed_sps$missing$values,
               c("-99", "-99"))
})

test_that("CPS 1973 has right missing values", {
  expect_equal(head(cps_1973_parsed_sps$missing$variable),
               c("V1013", "V1014", "V1020",
                 "V1021", "V1022", "V1029"))
  expect_equal(tail(cps_1973_parsed_sps$missing$variable),
               c("V1261", "V1262", "V1263",
                 "V1264", "V1265", "V1266"))

  expect_equal(head(cps_1973_parsed_sps$missing$values),
               c("0000000", "0000000", "0000000",
                 "0000000", "0000000", "0000000"))
  expect_equal(tail(cps_1973_parsed_sps$missing$values),
               c("-999999", "-999999", "-999999",
                 "-999999", "-999999", "-999999"))

  expect_equal(head(unique(cps_1973_parsed_sps$missing$variable)),
               c("V1013", "V1014", "V1020",
                 "V1021", "V1022", "V1029"))
  expect_equal(tail(unique(cps_1973_parsed_sps$missing$variable)),
               c("V1261", "V1262", "V1263",
                 "V1264", "V1265", "V1266"))
})


test_that("Census Police has right missing values", {
  expect_equal(head(census_police_parsed_sps$missing$variable),
               c("SUBTYPE1", "SUBTYPE2", "Q1A1",
                 "Q1A2", "Q1A3", "Q1A4"))
  expect_equal(tail(census_police_parsed_sps$missing$variable),
               c("Q6E", "Q6F", "Q6G",
                 "Q6H", "Q6I", "Q6_TOT"))

  expect_equal(head(census_police_parsed_sps$missing$values),
               c("888", "888", "-9",
                 "-9", "-9", "-9"))
  expect_equal(tail(census_police_parsed_sps$missing$values),
               c("-9", "-9", "-9",
                 "-9", "-9", "-9"))

  expect_equal(head(unique(census_police_parsed_sps$missing$variable)),
               c("SUBTYPE1", "SUBTYPE2", "Q1A1",
                 "Q1A2", "Q1A3", "Q1A4"))
  expect_equal(tail(unique(census_police_parsed_sps$missing$variable)),
               c("Q6E", "Q6F", "Q6G",
                 "Q6H", "Q6I", "Q6_TOT"))
})

test_that("India human has right missing values", {
  expect_equal(india_human_parsed_sps$missing$variable,
               "MB21B")
  expect_equal(tail(india_human_parsed_sps$missing$values),
               c("8"))
})



test_that("Sac has right missing values", {
  expect_equal(head(sac_parsed_sps$missing$variable),
               c("TODDATYR", "DATSTAR", "CONSTATE",
                 "Q3JETH", "Q5JSUPDP", "Q6JVIC"))
  expect_equal(head(sac_parsed_sps$missing$values),
               c("9999", "888888", "9",
                 "9", "9", "9"))
  expect_equal(tail(sac_parsed_sps$missing$variable),
               c("Q126PN3", "Q126OTH3", "KAGE",
                 "VERDICT", "DURAT", "DURAT2"))
  expect_equal(tail(sac_parsed_sps$missing$values),
               c("9", "99", "9",
                 "9", "99", "9"))

  expect_equal(head(unique(sac_parsed_sps$missing$variable)),
               c("TODDATYR", "DATSTAR", "CONSTATE",
                 "Q3JETH", "Q5JSUPDP", "Q6JVIC"))
  expect_equal(tail(unique(sac_parsed_sps$missing$variable)),
               c("Q126PN3", "Q126OTH3", "KAGE",
                 "VERDICT", "DURAT", "DURAT2"))
})

test_that("SADC has right missing values", {
  expect_equal(head(sadc_parsed_sps$missing$variable),
               c("sitecode", "sitetypenum", "year",
                 "survyear", "weight", "stratum"))
  expect_equal(head(sadc_parsed_sps$missing$values),
               c("", "", "",
                 "", "", ""))
  expect_equal(tail(sadc_parsed_sps$missing$variable),
               c("qnsunburn", "qnconcentrating", "qncurrentasthma",
                 "qnwheresleep", "qnspeakenglish", "qntransgender"))
  expect_equal(tail(sadc_parsed_sps$missing$values),
               c("", "", "",
                 "", "", ""))

  expect_equal(head(unique(sadc_parsed_sps$missing$variable)),
               c("sitecode", "sitetypenum", "year",
                 "survyear", "weight", "stratum"))
  expect_equal(tail(unique(sadc_parsed_sps$missing$variable)),
               c("qnsunburn", "qnconcentrating", "qncurrentasthma",
                 "qnwheresleep", "qnspeakenglish", "qntransgender"))
})

test_that("Sex offender has right missing values", {
  expect_equal(head(sex_offender_parsed_sps$missing$variable),
               c("DATE", "Q1", "Q3",
                 "Q4", "Q5", "Q7"))
  expect_equal(head(sex_offender_parsed_sps$missing$values),
               c("8888888", "99", "99",
                 "99", "99", "99"))
  expect_equal(tail(sex_offender_parsed_sps$missing$variable),
               c("Q9E", "Q9F", "Q9G",
                 "Q10", "INDEX", "NEWQ9G"))
  expect_equal(tail(sex_offender_parsed_sps$missing$values),
               c("99", "99", "99",
                 "99", "9", "9"))

  expect_equal(head(unique(sex_offender_parsed_sps$missing$variable)),
               c("DATE", "Q1", "Q3",
                 "Q4", "Q5", "Q7"))
  expect_equal(tail(unique(sex_offender_parsed_sps$missing$variable)),
               c("Q9E", "Q9F", "Q9G",
                 "Q10", "INDEX", "NEWQ9G"))
})

test_that("Weimar has right missing values", {
  expect_equal(head(weimar_parsed_sps$missing$variable),
               c("V5", "V6", "V7",
                 "V8", "V9", "V10"))
  expect_equal(head(weimar_parsed_sps$missing$values),
               c("9999999", "9999999", "999999.",
                 "9999999", "9999999", "9999999"))
  expect_equal(tail(weimar_parsed_sps$missing$variable),
               c("V18", "V19", "V20",
                 "V21", "V22", "V23"))
  expect_equal(tail(weimar_parsed_sps$missing$values),
               c("9999999", "999999.", "9999999",
                 "999999.", "9999999", "999999."))

  expect_equal(head(unique(weimar_parsed_sps$missing$variable)),
               c("V5", "V6", "V7",
                 "V8", "V9", "V10"))
  expect_equal(tail(unique(weimar_parsed_sps$missing$variable)),
               c("V18", "V19", "V20",
                 "V21", "V22", "V23"))
})

test_that("Parole has right missing values", {
  expect_equal(head(parole_parsed_sps$missing$variable),
               c("TOTBEG", "TOTBEG", "ENDISREL",
                 "ENDISREL", "ENMANREL", "ENMANREL"))
  expect_equal(head(parole_parsed_sps$missing$values),
               c("-8", "-9", "-8" ,
                 "-9", "-8", "-9"))
  expect_equal(tail(parole_parsed_sps$missing$variable),
               c("BOOTIN", "LOCJAIL", "LOCJAILIN",
                 "LOCJAILIN", "OTHPAR", "ENDOFYEAR"))
  expect_equal(tail(parole_parsed_sps$missing$values),
               c("NA", "DK", "DK",
                 "NA", "DK", "DK"))

  expect_equal(head(unique(parole_parsed_sps$missing$variable)),
               c("TOTBEG", "ENDISREL", "ENMANREL",
                 "ENREINST", "OTHEN", "TOTEN"))
  expect_equal(tail(unique(parole_parsed_sps$missing$variable)),
               c("BOOTNUM", "BOOTIN", "LOCJAIL",
                 "LOCJAILIN", "OTHPAR", "ENDOFYEAR"))
})

test_that("Prisoners has right missing values", {
  expect_equal(head(prisoners_parsed_sps$missing$variable),
               c("YEAR", "YEAR", "YEAR",
                 "YEAR", "YEAR", "YEAR"))
  expect_equal(head(prisoners_parsed_sps$missing$values),
               c("-9", "-8", "-7",
                 "-6", "-5", "-4"))
  expect_equal(tail(prisoners_parsed_sps$missing$variable),
               c("HANDLEF", "HANDLEF", "HANDLEF",
                 "HANDLEF", "HANDLEF", "HANDLEF"))
  expect_equal(tail(prisoners_parsed_sps$missing$values),
               c("-6", "-5", "-4",
                 "-3", "-2", "-1"))

  expect_equal(head(unique(prisoners_parsed_sps$missing$variable)),
               c("YEAR", "STATEID", "REGION",
                 "CUSGT1M", "CUSGT1F", "CUSLT1M"))
  expect_equal(tail(unique(prisoners_parsed_sps$missing$variable)),
               c("DTHOTHM", "DTHOTHF", "DTHTOTM",
                 "DTHTOTF", "HANDLEM", "HANDLEF"))
})

test_that("Crosswalk has right missing values", {
  expect_equal(head(crosswalk_parsed_sps$missing$variable),
               c("UORI", "UCOUNTY", "UMSA",
                 "UPOPGRP", "UADD5", "CGOVIDNU"))
  expect_equal(head(crosswalk_parsed_sps$missing$values),
               c("", "999", "999",
                 "", "99999", "999999999"))
  expect_equal(tail(crosswalk_parsed_sps$missing$variable),
               c("CGOVTYPE", "FSTATE", "FCOUNTY",
                 "FPLACE", "FMSA", "FCMSA"))
  expect_equal(tail(crosswalk_parsed_sps$missing$values),
               c("99", "99", "999",
                 "999999", "9999", "999"))

  expect_equal(head(unique(crosswalk_parsed_sps$missing$variable)),
               c("UORI", "UCOUNTY", "UMSA",
                 "UPOPGRP", "UADD5", "CGOVIDNU"))
  expect_equal(tail(unique(crosswalk_parsed_sps$missing$variable)),
               c("CGOVTYPE", "FSTATE", "FCOUNTY",
                 "FPLACE", "FMSA", "FCMSA"))
})

test_that("UCR 1985 has right missing values", {
  expect_equal(head(ucr1985_parsed_sps$missing$variable),
               c("V5", "V8", "V10",
                 "V11", "V11", "V12"))
  expect_equal(head(ucr1985_parsed_sps$missing$values),
               c("99", "99", "99",
                 "0", "99999", "0"))
  expect_equal(tail(ucr1985_parsed_sps$missing$variable),
               c("V169", "V170", "V171",
                 "V172", "V173", "V174"))
  expect_equal(tail(ucr1985_parsed_sps$missing$values),
               c("0", "0", "0",
                 "0", "0", "0"))

  expect_equal(head(unique(ucr1985_parsed_sps$missing$variable)),
               c("V5", "V8", "V10",
                 "V11", "V12", "V13"))
  expect_equal(tail(unique(ucr1985_parsed_sps$missing$variable)),
               c("V169", "V170", "V171",
                 "V172", "V173", "V174"))
})

test_that("UCR 1986 has right missing values", {
  expect_equal(head(ucr1986_parsed_sps$missing$variable),
               c("V4", "V5", "V7",
                 "V8", "V9", "V10"))
  expect_equal(head(ucr1986_parsed_sps$missing$values),
               c("0000000", "0000099", "0000000",
                 "0000099", "0000000", "0000099"))
  expect_equal(tail(ucr1986_parsed_sps$missing$variable),
               c("V169", "V170", "V171",
                 "V172", "V173", "V174"))
  expect_equal(tail(ucr1986_parsed_sps$missing$values),
               c("0000000", "0000000", "0000000",
                 "0000000", "0000000", "0000000"))

  expect_equal(head(unique(ucr1986_parsed_sps$missing$variable)),
               c("V4", "V5", "V7",
                 "V8", "V9", "V10"))
  expect_equal(tail(unique(ucr1986_parsed_sps$missing$variable)),
               c("V169", "V170", "V171",
                 "V172", "V173", "V174"))
})

test_that("Jail 2010 has right missing values", {
  expect_equal(head(jail_2010_parsed_sps$missing$variable),
               c("NONCITZF", "WEEK", "CONVII10A",
                 "CONVII10AF", "UNCONVII10A", "UNCONVII10AF"))
  expect_equal(head(jail_2010_parsed_sps$missing$values),
               c("-9", "-9", "-9",
                 "-9", "-9", "-9"))
  expect_equal(tail(jail_2010_parsed_sps$missing$variable),
               c("STOLENPROP", "STOLENPROPF", "ESCAPE",
                 "ESCAPEF", "OTHERMAJVIO", "OTHERMAJVIOF"))
  expect_equal(tail(jail_2010_parsed_sps$missing$values),
               c("-9", "-9", "-9",
                 "-9", "-9", "-9"))

  expect_equal(head(unique(jail_2010_parsed_sps$missing$variable)),
               c("NONCITZF", "WEEK", "CONVII10A",
                 "CONVII10AF", "UNCONVII10A", "UNCONVII10AF"))
  expect_equal(tail(unique(jail_2010_parsed_sps$missing$variable)),
               c("STOLENPROP", "STOLENPROPF", "ESCAPE",
                 "ESCAPEF", "OTHERMAJVIO", "OTHERMAJVIOF"))
})

test_that("Corrections has right missing values", {
  expect_equal(head(corrections_parsed_sps$missing$variable),
               c("EDUCATION", "ADMTYPE", "OFFGENERAL",
                 "SENTLGTH", "OFFDETAIL", "RACE"))
  expect_equal(head(corrections_parsed_sps$missing$values),
               c("9", "9", "9",
                 "9", "99", "9"))
  expect_equal(tail(corrections_parsed_sps$missing$variable),
               c("ADMTYPE", "OFFGENERAL", "SENTLGTH",
                 "OFFDETAIL", "RACE", "AGEADMIT"))
  expect_equal(tail(corrections_parsed_sps$missing$values),
               c("9", "9", "9",
                 "99", "9", "9"))

  expect_equal(head(unique(corrections_parsed_sps$missing$variable)),
               c("EDUCATION", "ADMTYPE", "OFFGENERAL",
                 "SENTLGTH", "OFFDETAIL", "RACE"))
  expect_equal(tail(unique(corrections_parsed_sps$missing$variable)),
               c("ADMTYPE", "OFFGENERAL", "SENTLGTH",
                 "OFFDETAIL", "RACE", "AGEADMIT"))
})


test_that("NCVS 1979 has right missing values", {
  expect_equal(head(ncvs_parsed_sps$missing$variable),
               c("V2009", "V2010", "V2012",
                 "V2014", "V2016", "V2018"))
  expect_equal(head(ncvs_parsed_sps$missing$values),
               c("9998 thru highest",
                 "8 thru highest",
                 "98 thru highest",
                 "8 thru highest",
                 "8 thru highest",
                 "8 thru highest"))

  expect_equal(tail(ncvs_parsed_sps$missing$variable),
               c("V3048", "V3049", "V3050",
                 "V3051", "V3052", "V3053"))
  expect_equal(tail(ncvs_parsed_sps$missing$values),
               c("98 thru highest",
                 "98 thru highest",
                 "98 thru highest",
                 "98 thru highest",
                 "98 thru highest",
                 "98 thru highest"))

  expect_equal(head(unique(ncvs_parsed_sps$missing$variable)),
               c("V2009", "V2010", "V2012",
                 "V2014", "V2016", "V2018"))
  expect_equal(tail(unique(ncvs_parsed_sps$missing$variable)),
               c("V3048", "V3049", "V3050",
                 "V3051", "V3052", "V3053"))
})


test_that("SHR 1988 has right missing values", {
  expect_equal(head(SHR1988_parsed_sps$missing$variable),
               c("V12", "V13", "V25",
                 "V26", "V27", "V28"))
  expect_equal(head(SHR1988_parsed_sps$missing$values),
               c("0", "0", "0",
                 "9", "9", "9"))
  expect_equal(tail(SHR1988_parsed_sps$missing$variable),
               c("V154", "V154", "V155",
                 "V155", "V156", "V156"))
  expect_equal(tail(SHR1988_parsed_sps$missing$values),
               c("98", "99", "98",
                 "99", "8 THRU HI", "7"))

  expect_equal(head(unique(SHR1988_parsed_sps$missing$variable)),
               c("V12", "V13", "V25",
                 "V26", "V27", "V28"))
  expect_equal(tail(unique(SHR1988_parsed_sps$missing$variable)),
               c("V151", "V152", "V153",
                 "V154", "V155", "V156"))
})


test_that("SHR 1987 has right missing values", {
  expect_equal(head(SHR1987_parsed_sps$missing$variable),
               c("V12", "V13", "V25",
                 "V26", "V27", "V28"))
  expect_equal(head(SHR1987_parsed_sps$missing$values),
               c("0", "0", "0",
                 "9", "9", "9"))
  expect_equal(tail(SHR1987_parsed_sps$missing$variable),
               c("V154", "V154", "V155",
                 "V155", "V156", "V156"))
  expect_equal(tail(SHR1987_parsed_sps$missing$values),
               c("98", "99", "98",
                 "99", "8 THRU HI", "7"))

  expect_equal(head(SHR1987_parsed_sps$missing$variable),
               c("V12", "V13", "V25",
                 "V26", "V27", "V28"))
  expect_equal(tail(unique(SHR1987_parsed_sps$missing$variable)),
               c("V151", "V152", "V153",
                 "V154", "V155", "V156"))
})

test_that("UCR 1985 has right missing values", {
  expect_equal(head(ucr1985_parsed_sps$missing$variable),
               c("V5", "V8", "V10",
                 "V11", "V11", "V12"))
  expect_equal(head(ucr1985_parsed_sps$missing$values),
               c("99", "99", "99",
                 "0", "99999", "0"))
  expect_equal(tail(ucr1985_parsed_sps$missing$variable),
               c("V169", "V170", "V171",
                 "V172", "V173", "V174"))
  expect_equal(tail(ucr1985_parsed_sps$missing$values),
               c("0", "0", "0",
                 "0", "0", "0"))

  expect_equal(head(unique(ucr1985_parsed_sps$missing$variable)),
               c("V5", "V8", "V10",
                 "V11", "V12", "V13"))
  expect_equal(tail(ucr1985_parsed_sps$missing$variable),
               c("V169", "V170", "V171",
                 "V172", "V173", "V174"))
})



test_that("Dutch election has right missing values", {
  expect_equal(head(dutch_election_parsed_sps$missing$variable),
               c("V6", "V6", "V7",
                 "V7", "V8", "V10"))
  expect_equal(head(dutch_election_parsed_sps$missing$values),
               c("0000009 THRU HI", "0000000", "0000009 THRU HI",
                 "0000000", "0000000", "0011499 THRU HI"))
  expect_equal(tail(dutch_election_parsed_sps$missing$variable),
               c("V763", "V763", "V764",
                 "V764", "V765", "V765"))
  expect_equal(tail(dutch_election_parsed_sps$missing$values),
               c("0000098 THRU HI", "0000000", "0000009 THRU HI",
                 "0000000", "0000009 THRU HI", "0000000"))

  expect_equal(head(unique(dutch_election_parsed_sps$missing$variable)),
               c("V6", "V7", "V8",
                 "V10", "V30", "V31"))
  expect_equal(tail(unique(dutch_election_parsed_sps$missing$variable)),
               c("V760", "V761", "V762",
                 "V763", "V764", "V765"))
})






test_that("Monitoring the Future 2003 has right missing values", {
  expect_equal(head(mtf_2003_parsed_sps$missing$variable),
               c("CASEID", "V13", "V16",
                 "V17", "V5", "V1"))
  expect_equal(head(mtf_2003_parsed_sps$missing$values),
               c("-9", "-9", "-9",
                 "-9", "-9", "-9"))

  expect_equal(tail(mtf_2003_parsed_sps$missing$variable),
               c("V112", "V113", "V114",
                 "V205", "V206", "V207"))
  expect_equal(tail(mtf_2003_parsed_sps$missing$values),
               c("-9", "-9", "-9",
                 "-9", "-9", "-9"))

  expect_equal(head(unique(mtf_2003_parsed_sps$missing$variable)),
               c("CASEID", "V13", "V16",
                 "V17", "V5", "V1"))
  expect_equal(tail(unique(mtf_2003_parsed_sps$missing$variable)),
               c("V112", "V113", "V114",
                 "V205", "V206", "V207"))
})



test_that("Monitoring the Future 1990 has right missing values", {
  expect_equal(head(mtf_1990_parsed_sps$missing$variable),
               c("V1", "V3", "V4",
                 "V4", "V5", "V5"))
  expect_equal(head(mtf_1990_parsed_sps$missing$values),
               c("99", "9", "99999",
                 "99999 THRU HIGHEST", "0", "9"))

  expect_equal(tail(mtf_1990_parsed_sps$missing$variable),
               c("V145", "V145", "V146",
                 "V146", "V147", "V147"))
  expect_equal(tail(mtf_1990_parsed_sps$missing$values),
               c("0", "9", "0",
                 "9", "0", "9"))

  expect_equal(head(unique(mtf_1990_parsed_sps$missing$variable)),
               c("V1", "V3", "V4",
                 "V5", "V13", "V16"))
  expect_equal(tail(unique(mtf_1990_parsed_sps$missing$variable)),
               c("V142", "V143", "V144",
                 "V145", "V146", "V147"))
})



test_that("Monitoring the Future 1989 has right missing values", {
  expect_equal(head(mtf_1989_parsed_sps$missing$variable),
               c("V1", "V1", "V3",
                 "V3", "V4", "V4"))
  expect_equal(head(mtf_1989_parsed_sps$missing$values),
               c("0000099", "0000099 THRU HIGHEST", "0000009",
                 "0000009 THRU HIGHEST", "0099999", "0099999 THRU HIGHEST"))

  expect_equal(tail(mtf_1989_parsed_sps$missing$variable),
               c("V205", "V205", "V206",
                 "V206", "V207", "V207"))
  expect_equal(tail(mtf_1989_parsed_sps$missing$values),
               c("0000000", "0000008 THRU HIGHEST", "0000000",
                 "0000008 THRU HIGHEST", "0000000", "0000008 THRU HIGHEST"))

  expect_equal(head(unique(mtf_1989_parsed_sps$missing$variable)),
               c("V1", "V3", "V4",
                 "V5", "V13", "V16"))
  expect_equal(tail(unique(mtf_1989_parsed_sps$missing$variable)),
               c("V202", "V203", "V204",
                 "V205", "V206", "V207"))
})



test_that("Monitoring the Future 2004 has right missing values", {
  expect_equal(head(mtf_2004_parsed_sps$missing$variable),
               c("V1", "V1", "V3",
                 "V3", "V4", "V4"))
  expect_equal(head(mtf_2004_parsed_sps$missing$values),
               c("99 THRU HI", "99", "9 THRU HI",
                 "9", "99999 THRU HI", "99999"))

  expect_equal(tail(mtf_2004_parsed_sps$missing$variable),
               c("V9001", "V9001", "V9002",
                 "V9002", "V9003", "V9003"))
  expect_equal(tail(mtf_2004_parsed_sps$missing$values),
               c("9998 THRU HI", "9999", "8 THRU HI",
                 "9", "8 THRU HI", "9"))

  expect_equal(head(unique(mtf_2004_parsed_sps$missing$variable)),
               c("V1", "V3", "V4",
                 "V5", "V13", "V16"))
  expect_equal(tail(unique(mtf_2004_parsed_sps$missing$variable)),
               c("V205", "V206", "V207",
                 "V9001", "V9002", "V9003"))
})



test_that("Monitoring the Future 2002 has right missing values", {
  expect_equal(head(mtf_2002_parsed_sps$missing$variable),
               c("V13", "V16", "V17",
                 "V5", "V1", "V3"))
  expect_equal(head(mtf_2002_parsed_sps$missing$values),
               c("-9", "-9", "-9",
                 "-9", "-9", "-9"))

  expect_equal(tail(mtf_2002_parsed_sps$missing$variable),
               c("V113", "V114", "V205",
                 "V206", "V207", "CASEID"))
  expect_equal(tail(mtf_2002_parsed_sps$missing$values),
               c("-9", "-9", "-9",
                 "-9", "-9", "-9"))

  expect_equal(head(unique(mtf_2002_parsed_sps$missing$variable)),
               c("V13", "V16", "V17",
                 "V5", "V1", "V3"))
  expect_equal(tail(unique(mtf_2002_parsed_sps$missing$variable)),
               c("V113", "V114", "V205",
                 "V206", "V207", "CASEID"))
})


test_that("Monitoring the Future 1993 has right missing values", {
  expect_equal(head(mtf_1993_parsed_sps$missing$variable),
               c("V1", "V3", "V4",
                 "V4", "V5", "V5"))
  expect_equal(head(mtf_1993_parsed_sps$missing$values),
               c("99", "9", "99999",
                 "99999 THRU HIGHEST", "0", "9"))

  expect_equal(tail(mtf_1993_parsed_sps$missing$variable),
               c("V145", "V145", "V146",
                 "V146", "V147", "V147"))
  expect_equal(tail(mtf_1993_parsed_sps$missing$values),
               c("0", "9", "0",
                 "9", "0", "9"))

  expect_equal(head(unique(mtf_1993_parsed_sps$missing$variable)),
               c("V1", "V3", "V4",
                 "V5", "V13", "V16"))
  expect_equal(tail(unique(mtf_1993_parsed_sps$missing$variable)),
               c("V142", "V143", "V144",
                 "V145", "V146", "V147"))
})


test_that("Monitoring the Future 1991 has right missing values", {
  expect_equal(head(mtf_1991_parsed_sps$missing$variable),
               c("V1", "V3", "V4",
                 "V4", "V5", "V5"))
  expect_equal(head(mtf_1991_parsed_sps$missing$values),
               c("99", "9", "99999",
                 "99999 THRU HIGHEST", "0", "9"))

  expect_equal(tail(mtf_1991_parsed_sps$missing$variable),
               c("V145", "V145", "V146",
                 "V146", "V147", "V147"))
  expect_equal(tail(mtf_1991_parsed_sps$missing$values),
               c("0", "9", "0",
                 "9", "0", "9"))

  expect_equal(head(unique(mtf_1991_parsed_sps$missing$variable)),
               c("V1", "V3", "V4",
                 "V5", "V13", "V16"))
  expect_equal(tail(unique(mtf_1991_parsed_sps$missing$variable)),
               c("V142", "V143", "V144",
                 "V145", "V146", "V147"))
})

test_that("Monitoring the Future 1992 has right missing values", {
  expect_equal(head(mtf_1992_parsed_sps$missing$variable),
               c("V1", "V3", "V4",
                 "V4", "V5", "V5"))
  expect_equal(head(mtf_1992_parsed_sps$missing$values),
               c("99", "9", "99999",
                 "99999 THRU HIGHEST", "0", "9"))

  expect_equal(tail(mtf_1992_parsed_sps$missing$variable),
               c("V145", "V145", "V146",
                 "V146", "V147", "V147"))
  expect_equal(tail(mtf_1992_parsed_sps$missing$values),
               c("0", "9", "0",
                 "9", "0", "9"))

  expect_equal(head(unique(mtf_1992_parsed_sps$missing$variable)),
               c("V1", "V3", "V4",
                 "V5", "V13", "V16"))
  expect_equal(tail(unique(mtf_1992_parsed_sps$missing$variable)),
               c("V142", "V143", "V144",
                 "V145", "V146", "V147"))
})

test_that("Monitoring the Future 1979 has right missing values", {
  expect_equal(head(mtf_1979_parsed_sps$missing$variable),
               c("V5", "V5", "V13",
                 "V16", "V17", "V4101"))
  expect_equal(head(mtf_1979_parsed_sps$missing$values),
               c("9 THRU HI", "0", "9 THRU HI",
                 "9 THRU HI", "9 THRU HI", "9 THRU HI"))

  expect_equal(tail(mtf_1979_parsed_sps$missing$variable),
               c("V4382", "V4382", "V4383",
                 "V4383", "V4384", "V4384"))
  expect_equal(tail(mtf_1979_parsed_sps$missing$values),
               c("9 THRU HI", "0", "9 THRU HI",
                 "0", "9 THRU HI", "0"))

  expect_equal(head(unique(mtf_1979_parsed_sps$missing$variable)),
               c("V5", "V13", "V16",
                 "V17", "V4101", "V4102"))
  expect_equal(tail(unique(mtf_1979_parsed_sps$missing$variable)),
               c("V4379", "V4380", "V4381",
                 "V4382", "V4383", "V4384"))
})

