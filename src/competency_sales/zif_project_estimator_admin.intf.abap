interface ZIF_PROJECT_ESTIMATOR_ADMIN
  public .


  methods SET_TEMPLATE
    importing
      !IV_TEMPLATE_NAME type CHAR12
    raising
      ZCX_PROJECT_ESTIMATOR .
  methods SET_SCHEMA
    importing
      !IV_SCHEMA_NAME type CHAR20 .
endinterface.
