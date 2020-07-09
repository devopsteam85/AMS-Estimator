class ZCL_PROJECT_ESTIMATOR_FACTORY definition
  public
  final
  create public .

public section.

  class-methods GET_ESTIMATOR_INSTANCE
    importing
      !IV_PROJECT_TYPE type CHAR10
    returning
      value(RV_INSTANCE) type ref to ZIF_PROJECT_ESTIMATOR .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_PROJECT_ESTIMATOR_FACTORY IMPLEMENTATION.


  METHOD get_estimator_instance.
    CASE iv_project_type.
      WHEN 'AMS'.
        CREATE OBJECT rv_instance TYPE zcl_project_estimator_ams.
      WHEN 'AD'.
        CREATE OBJECT rv_instance TYPE zcl_project_estimator_ad.
      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
