class ZCL_PROJECT_SCHEMA_PRIORITY definition
  public
  final
  create public .

public section.

  interfaces ZIF_PROJECT_ESTIMATOR_SCHEMA .
  interfaces ZIF_PROJECT_ESTIMATOR_ATTR .

  data LINES type SYTABIX .

  methods CONSTRUCTOR
    importing
      !IV_KEY type STRING
      !IO_PARENT type ref to ZIF_PROJECT_ESTIMATOR_SCHEMA
      !IT_DUMP type ref to DATA
      !IT_SPREAD type ZIF_PROJECT_ESTIMATOR_ATTR~TT_LEVEL_SPREAD .
protected section.
private section.

  data KEY type CHAR20 .
  data GT_LEVEL_SPREAD type ZIF_PROJECT_ESTIMATOR_ATTR~TT_LEVEL_SPREAD .
ENDCLASS.



CLASS ZCL_PROJECT_SCHEMA_PRIORITY IMPLEMENTATION.


  METHOD constructor.

    DATA: lt_dump_tab TYPE STANDARD TABLE OF zif_project_estimator_attr~ty_dump,
          lt_records  TYPE STANDARD TABLE OF zif_project_estimator_attr~ty_dump.
    FIELD-SYMBOLS: <lft_dump> TYPE STANDARD TABLE.

    key = iv_key.
    zif_project_estimator_schema~key = iv_key.
    zif_project_estimator_schema~parent ?= io_parent.
    zif_project_estimator_schema~schema_name = 'PRIORITY'.


    ASSIGN it_dump->* TO <lft_dump>.
    lt_dump_tab = <lft_dump>.
    zcl_project_estimator_engine=>zif_project_estimator_engine~get_records( EXPORTING it_dump = lt_dump_tab
                                 iv_value =  key
                                 iv_field = 'PRIORITY'
                       IMPORTING et_records = lt_records ).
*   Set total number of records for the priority
    lines = lines( lt_records ).


    zcl_project_estimator_engine=>zif_project_estimator_engine~get_records( EXPORTING it_dump = it_spread
                                 iv_value =  key
                                 iv_field = 'PRIORITY'
                       IMPORTING et_records = gt_level_spread ).
  ENDMETHOD.


  METHOD zif_project_estimator_schema~get_key_descriptor.
    rs_component-name = zif_project_estimator_schema~schema_name.
    rs_component-type ?= cl_abap_datadescr=>describe_by_data( key ).
  ENDMETHOD.


  METHOD zif_project_estimator_schema~get_parent.
    ro_parent ?= zif_project_estimator_schema~parent.
  ENDMETHOD.


  METHOD zif_project_estimator_schema~get_schema_descriptor.
    APPEND zif_project_estimator_schema~get_key_descriptor( ) TO ct_components.

    READ TABLE zif_project_estimator_schema~children INTO DATA(ls_child) INDEX 1.
    IF sy-subrc EQ 0.
      ls_child-child->get_schema_descriptor( CHANGING ct_components = ct_components ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_project_estimator_schema~get_schema_output.

    ASSERT it_schema_output IS BOUND.

    FIELD-SYMBOLS: <lt_schema_output>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <lfs_schema_out> TYPE any,
                   <lfs_value>      TYPE any.

    ASSIGN it_schema_output->* TO <lt_schema_output>.

    LOOP AT zif_project_estimator_schema~children INTO DATA(ls_child).

      READ TABLE <lt_schema_output> ASSIGNING <lfs_schema_out> INDEX lines( <lt_schema_output> ).
      ASSIGN COMPONENT zif_project_estimator_schema~schema_name OF STRUCTURE <lfs_schema_out> TO <lfs_value>.

      <lfs_value> = key.
      DATA(lo_child) = ls_child-child.
      AT FIRST.
        lo_child->get_schema_output( EXPORTING it_schema_output = it_schema_output
                                     IMPORTING et_schema_output = et_schema_output ).
        CONTINUE.
      ENDAT.

      APPEND  <lfs_schema_out> TO <lt_schema_output>.

      lo_child->get_schema_output( EXPORTING it_schema_output = it_schema_output
                                         IMPORTING et_schema_output = et_schema_output ).

    ENDLOOP.
  ENDMETHOD.


  METHOD zif_project_estimator_schema~invoke_schema.
    DATA: lv_key TYPE string.
    LOOP AT gt_level_spread INTO DATA(ls_spread).
      lv_key = ls_spread-level.
      zif_project_estimator_schema~set_child(
                             NEW zcl_project_schema_level(
                                iv_key = lv_key
                                io_parent = me
                                iv_SPREAD = ls_spread-spread
                                 ) ).
    ENDLOOP.
    LOOP AT zif_project_estimator_schema~children INTO DATA(ls_child).
      ls_child-child->invoke_schema( ).
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_project_estimator_schema~run_schema.
    LOOP AT zif_project_estimator_schema~children INTO DATA(ls_child).

      ls_child-child->run_schema( ).
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_project_estimator_schema~set_child.
    DATA: ls_child TYPE zif_project_estimator_schema~ty_child.
    ls_child-child ?= io_child.
    APPEND ls_child TO zif_project_estimator_schema~children.
  ENDMETHOD.
ENDCLASS.
