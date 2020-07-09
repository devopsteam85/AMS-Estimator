class ZCL_PROJECT_SCHEMA_MODULES definition
  public
  final
  create public .

public section.

  interfaces ZIF_PROJECT_ESTIMATOR_SCHEMA .
  interfaces ZIF_PROJECT_ESTIMATOR_ATTR .

  data GT_LEVEL_SPREAD type ZIF_PROJECT_ESTIMATOR_ATTR~TT_LEVEL_SPREAD .

  methods CONSTRUCTOR
    importing
      !IV_KEY type STRING
      !IO_PARENT type ref to ZIF_PROJECT_ESTIMATOR_SCHEMA
      !IT_DUMP type ref to DATA
      !IT_CONSULTANT_SPREAD type ZIF_PROJECT_ESTIMATOR_ATTR~TT_CONSULTANT_SPREAD .
  methods GET_CONSULTANT_SPREAD
    exporting
      !RT_CONSULTANT_SPREAD type ZIF_PROJECT_ESTIMATOR_ATTR~TT_CONSULTANT_SPREAD .
protected section.
private section.

  data KEY type CHAR20 .
  data GT_RECORDS type ZIF_PROJECT_ESTIMATOR_ATTR~TT_DUMP .
  data GT_CONSULTANT_SPREAD type ZIF_PROJECT_ESTIMATOR_ATTR~TT_CONSULTANT_SPREAD .
ENDCLASS.



CLASS ZCL_PROJECT_SCHEMA_MODULES IMPLEMENTATION.


  METHOD constructor.


    DATA: lt_dump_tab TYPE STANDARD TABLE OF zif_project_estimator_attr~ty_dump.

    FIELD-SYMBOLS: <lft_dump> TYPE STANDARD TABLE.

    DATA: lt_spread TYPE zif_project_estimator_attr~tt_level_spread,
          ls_spread TYPE zif_project_estimator_attr~ty_level_spread,
          lv_xml    TYPE string,
          lo_input  TYPE REF TO zcl_abapgit_xml_input.

    key = iv_key.
    zif_project_estimator_schema~key = iv_key.
    zif_project_estimator_schema~parent ?= io_parent.
    zif_project_estimator_schema~schema_name = 'MODULE'.

    ASSIGN it_dump->* TO <lft_dump>.
    lt_dump_tab = <lft_dump>.
    zcl_project_estimator_engine=>zif_project_estimator_engine~get_records( EXPORTING it_dump = lt_dump_tab
                                 iv_value =  key
                                iv_field = 'MODULE'
                     IMPORTING et_records = gt_records ).

    zcl_project_estimator_engine=>zif_project_estimator_engine~get_records( EXPORTING it_dump = it_consultant_spread
                                 iv_value =  key
                                 iv_field = 'MODULE'
                       IMPORTING et_records = gt_consultant_spread ).




*   Set Key
    zif_project_estimator_attr~gs_project_key-project = 'AMS'.
    zif_project_estimator_attr~gs_project_key-type    = 'SCHEMA'.
    zif_project_estimator_attr~gs_project_key-value   = 'DEFAULT'.
*
*    go_db->set_key( iv_key = zif_project_estimator_attr~gs_project_key ).

    SELECT SINGLE data FROM zprojest INTO lv_xml
    WHERE project = zif_project_estimator_attr~gs_project_key-project
    AND   type    = zif_project_estimator_attr~gs_project_key-type
    AND   value   =  zif_project_estimator_attr~gs_project_key-value.
    IF sy-subrc EQ 0.

*   Render the XML
      CREATE OBJECT lo_input
        EXPORTING
          iv_xml = lv_xml.

*   Read the consultant spread from the Schema XML
      lo_input->read( EXPORTING iv_name = 'LEVEL'
                  CHANGING cg_data = gt_level_spread ).

    ENDIF.

*   Use hardcoded values if schema is not maintained for this project type
*    CHECK gt_level_spread IS INITIAL.
*
*    ls_spread-priority  = 'P1'.
*    ls_spread-level   = 'L1'.
*    ls_spread-spread  = 75.
*    APPEND ls_spread TO lt_spread.
*
*    ls_spread-priority  = 'P1'.
*    ls_spread-level   = 'L2'.
*    ls_spread-spread  = 23.
*    APPEND ls_spread TO lt_spread.
*
*
*    ls_spread-priority  = 'P1'.
*    ls_spread-level   = 'L3'.
*    ls_spread-spread  = 2.
*    APPEND ls_spread TO lt_spread.
*
*
*
** P2
*
*    ls_spread-priority  = 'P2'.
*    ls_spread-level   = 'L1'.
*    ls_spread-spread  = 25.
*    APPEND ls_spread TO lt_spread.
*
*    ls_spread-priority  = 'P2'.
*    ls_spread-level   = 'L2'.
*    ls_spread-spread  = 60.
*    APPEND ls_spread TO lt_spread.
*
*
*    ls_spread-priority  = 'P2'.
*    ls_spread-level   = 'L3'.
*    ls_spread-spread  = 10.
*    APPEND ls_spread TO lt_spread.
*
*    ls_spread-priority  = 'P2'.
*    ls_spread-level   = 'L4'.
*    ls_spread-spread  = 5.
*    APPEND ls_spread TO lt_spread.
*
** P3
*
*    ls_spread-priority  = 'P3'.
*    ls_spread-level   = 'L1'.
*    ls_spread-spread  = 25.
*    APPEND ls_spread TO lt_spread.
*
*    ls_spread-priority  = 'P3'.
*    ls_spread-level   = 'L2'.
*    ls_spread-spread  = 50.
*    APPEND ls_spread TO lt_spread.
*
*    ls_spread-priority  = 'P3'.
*    ls_spread-level   = 'L3'.
*    ls_spread-spread  = 20.
*    APPEND ls_spread TO lt_spread.
*
*    ls_spread-priority  = 'P3'.
*    ls_spread-level   = 'L4'.
*    ls_spread-spread  = 5.
*    APPEND ls_spread TO lt_spread.
*
** P4
*
*    ls_spread-priority  = 'P4'.
*    ls_spread-level   = 'L1'.
*    ls_spread-spread  = 20.
*    APPEND ls_spread TO lt_spread.
*
*    ls_spread-priority  = 'P4'.
*    ls_spread-level   = 'L2'.
*    ls_spread-spread  = 40.
*    APPEND ls_spread TO lt_spread.
*
*    ls_spread-priority  = 'P4'.
*    ls_spread-level   = 'L3'.
*    ls_spread-spread  = 30.
*    APPEND ls_spread TO lt_spread.
*
*    ls_spread-priority  = 'P4'.
*    ls_spread-level   = 'L4'.
*    ls_spread-spread  = 10.
*    APPEND ls_spread TO lt_spread.
*
*
*
*
*
*
*
*
*
*    gt_level_spread = lt_spread.
  ENDMETHOD.


  METHOD get_consultant_spread.
    rt_consultant_spread  = gt_consultant_spread.
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
    DATA: lt_priority TYPE zif_project_estimator_attr~tt_priority,
          lv_key      TYPE string,
          lt_dump     TYPE REF TO data.

    zcl_project_estimator_engine=>zif_project_estimator_engine~get_distinct( EXPORTING it_dump = gt_records
                                                         iv_field = 'PRIORITY'
                                               IMPORTING et_distinct = lt_priority ).

    GET REFERENCE OF gt_records INTO lt_dump.
    LOOP AT lt_priority INTO DATA(ls_priority).
      lv_key = ls_priority.
      zif_project_estimator_schema~set_child(
                             NEW zcl_project_schema_priority(
                                iv_key = lv_key
                                io_parent = me
                                it_dump  = lt_dump
                                it_spread = gt_level_spread ) ).
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
