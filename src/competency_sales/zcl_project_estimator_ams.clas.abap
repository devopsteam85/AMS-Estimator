class ZCL_PROJECT_ESTIMATOR_AMS definition
  public
  inheriting from ZCL_PROJECT_ESTIMATOR
  create public .

public section.

  methods CONSTRUCTOR .

  methods ZIF_PROJECT_ESTIMATOR_INPUT~LOAD_DUMP
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_PROJECT_ESTIMATOR_AMS IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).
    go_db->zif_project_estimator_attr~g_project = zif_project_estimator_attr~g_project = zif_project_estimator_attr~c_project_ams.
*   Loads the project specific data as instance for the class
    zif_project_estimator_engine~handle_action( 'LOAD_PROJECT' ).

  ENDMETHOD.


  METHOD zif_project_estimator_input~load_dump.
  data: lt_dump type ref to data.
    CALL METHOD super->zif_project_estimator_input~load_dump.

    go_engine = NEW zcl_project_estimator_engine( it_dump = gt_dump ).
*    raise event process_dump exporting io_estimator = me .

    go_engine->process_dump( io_estimator = me ).
*    go_engine->get_output( ).
  ENDMETHOD.
ENDCLASS.
