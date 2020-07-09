interface ZIF_PROJECT_ESTIMATOR_ENGINE
  public .


  interfaces ZIF_PROJECT_ESTIMATOR_ATTR .

  class-methods GET_DISTINCT
    importing
      !IV_FIELD type CLIKE
      value(IT_DUMP) type STANDARD TABLE
    exporting
      !ET_DISTINCT type STANDARD TABLE .
  class-methods GET_RECORDS
    importing
      value(IT_DUMP) type STANDARD TABLE
      !IV_VALUE type DATA
      !IV_FIELD type CLIKE
    exporting
      !ET_RECORDS type STANDARD TABLE .
  methods GET_RECORD .
  methods PROCESS_RECORD .
  methods HANDLE_ACTION
    importing
      !IV_ACTION type CHAR25 .
  methods PROCESS_DUMP
    for event PROCESS_DUMP of ZCL_PROJECT_ESTIMATOR
    importing
      !IO_ESTIMATOR .
  methods GET_UNIQUE_MODULES
    importing
      !IT_DUMP type ref to DATA
    exporting
      value(ET_MODULES) type ZIF_PROJECT_ESTIMATOR_ATTR~TT_PROJEST .
  methods GET_UNIQUE_PRIORITY
    importing
      !IT_DUMP type ref to DATA
    exporting
      !ET_PRIORITY type ZIF_PROJECT_ESTIMATOR_ATTR~TT_PRIORITY .
  methods GET_OUTPUT
    changing
      !CT_OUPUT type ref to DATA .
endinterface.
