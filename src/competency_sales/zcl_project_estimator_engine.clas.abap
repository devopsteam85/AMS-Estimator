class ZCL_PROJECT_ESTIMATOR_ENGINE definition
  public
  final
  create public .

public section.

  interfaces ZIF_PROJECT_ESTIMATOR_ATTR .
  interfaces ZIF_PROJECT_ESTIMATOR_ENGINE .

  methods CONSTRUCTOR
    importing
      !IT_DUMP type ref to DATA .
protected section.
private section.

  data GT_DUMP type ref to DATA .
  data GT_MODULES type ZIF_PROJECT_ESTIMATOR_ATTR~TT_MODULE .
  data GT_PRIORITY type ZIF_PROJECT_ESTIMATOR_ATTR~TT_PRIORITY .
  data GO_SCHEMA type ref to ZCL_PROJECT_ESTIMATOR_SCHEMA .
ENDCLASS.



CLASS ZCL_PROJECT_ESTIMATOR_ENGINE IMPLEMENTATION.


  METHOD constructor.
    gt_dump = it_dump.
  ENDMETHOD.


  METHOD zif_project_estimator_engine~get_distinct.

    FIELD-SYMBOLS: <lfs_dump>  TYPE STANDARD TABLE,
                   <lfs_value> TYPE any.
*                   <lfs_record> like line of <lfs_dump>.


    ASSIGN it_dump TO <lfs_dump>.
    IF <lfs_dump> IS ASSIGNED.
      SORT <lfs_dump> BY (iv_field).
      DELETE ADJACENT DUPLICATES FROM <lfs_dump> COMPARING  (iv_field).
      LOOP AT <lfs_dump> ASSIGNING FIELD-SYMBOL(<lfs_record>).

        ASSIGN COMPONENT iv_field OF STRUCTURE <lfs_record> TO <lfs_value>.
        IF <lfs_value> IS ASSIGNED.

          APPEND INITIAL LINE TO et_distinct ASSIGNING FIELD-SYMBOL(<lfs_distinct>).
          <lfs_distinct> = <lfs_value>.
        ENDIF.


      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD zif_project_estimator_engine~get_records.
    FIELD-SYMBOLS: <lfs_dump>  TYPE STANDARD TABLE,
                   <lfs_value> TYPE any.
*                   <lfs_record> like line of <lfs_dump>.


    ASSIGN it_dump TO <lfs_dump>.
    IF <lfs_dump> IS ASSIGNED.
      SORT <lfs_dump> BY (iv_field).
      READ TABLE <lfs_dump> WITH KEY (iv_field) =  iv_value TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        DATA(l_tabix) = sy-tabix.
      ENDIF.
      LOOP AT <lfs_dump> ASSIGNING FIELD-SYMBOL(<lfs_record>) FROM l_tabix.
        ASSIGN COMPONENT iv_field OF STRUCTURE <lfs_record> TO <lfs_value>.
        IF <lfs_value> IS ASSIGNED.
          IF <lfs_value> NE iv_value.
            EXIT.
          ELSE.
            APPEND INITIAL LINE TO et_records ASSIGNING FIELD-SYMBOL(<lfs_records>).
            <lfs_records> = <lfs_record>.
          ENDIF.
        ENDIF.
*       DELETE <lfs_dump> WHERE (iv_field) ne iv_value.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  method ZIF_PROJECT_ESTIMATOR_ENGINE~HANDLE_ACTION.
  endmethod.


  METHOD zif_project_estimator_engine~process_dump.

    go_schema = NEW zcl_project_estimator_schema( it_dump = gt_dump ).


    go_schema->zif_project_estimator_schema~invoke_schema( ).

    go_schema->zif_project_estimator_schema~run_schema( ).
    DATA: lt_schema_output TYPE REF TO data.
    go_schema->zif_project_estimator_schema~get_schema_output( EXPORTING it_schema_output = lt_schema_output ).


*  Get Months
*  Get Schema
*  Get ticket counts for Moudle/Priority
*  Compute spread for Module priority
*  Compute Efforts for Spread
*  Compute FTE for Efforts

  ENDMETHOD.
ENDCLASS.
