interface ZIF_PROJECT_ESTIMATOR_DB
  public .


  interfaces ZIF_PROJECT_ESTIMATOR_ATTR .

  methods LOAD_PROJECT
    raising
      resumable(ZCX_PROJECT_ESTIMATOR) .
  methods PROCESS_RECORD
    raising
      ZCX_PROJECT_ESTIMATOR .
  methods READ_KEY_DATA
    returning
      value(RV_KEY_RECORD) type ZPROJEST
    raising
      resumable(ZCX_PROJECT_ESTIMATOR) .
  methods SET_KEY
    importing
      !IV_KEY type ZPROJEST .
  methods GET_KEY
    returning
      value(RV_KEY) type ZPROJEST .
  methods UPDATE_KEY_DATA
    importing
      !IV_KEY_RECORD type ZPROJEST .
  methods UPDATE_DB .
endinterface.
