class ZCL_PROJECT_SCHEMA_LEVEL definition
  public
  final
  create public .

public section.

  interfaces ZIF_PROJECT_ESTIMATOR_SCHEMA .
  interfaces ZIF_PROJECT_ESTIMATOR_ATTR .

  data GV_SPREAD_COUNT(7) type p decimals 2 .

  methods CONSTRUCTOR
    importing
      !IV_KEY type STRING
      !IO_PARENT type ref to ZIF_PROJECT_ESTIMATOR_SCHEMA
      !IV_SPREAD type NUMC3 .
protected section.
private section.

  data GV_SPREAD type NUMC3 .
  data KEY type CHAR20 .
  data GT_CONSULTANT_SPREAD type ZIF_PROJECT_ESTIMATOR_ATTR~TT_CONSULTANT_SPREAD .
ENDCLASS.



CLASS ZCL_PROJECT_SCHEMA_LEVEL IMPLEMENTATION.


  METHOD constructor.

    DATA: lo_parent            TYPE REF TO zcl_project_schema_modules,
          lt_consultant_spread TYPE zif_project_estimator_attr~tt_consultant_spread.

    key = iv_key.
    zif_project_estimator_schema~key = iv_key.
    zif_project_estimator_schema~parent ?= io_parent.
    zif_project_estimator_schema~schema_name = 'LEVEL'.
    gv_spread = iv_spread.


    lo_parent ?= io_parent->get_parent( ).

    lo_parent->get_consultant_spread( IMPORTING rt_consultant_spread = lt_consultant_spread ).

    zcl_project_estimator_engine=>zif_project_estimator_engine~get_records( EXPORTING it_dump = lt_consultant_spread
                            iv_value =  key
                            iv_field = 'LEVEL'
                  IMPORTING et_records = gt_consultant_spread ).

  ENDMETHOD.


  METHOD zif_project_estimator_schema~compute.

    DATA: lo_priority TYPE REF TO zcl_project_schema_priority.

    lo_priority ?= zif_project_estimator_schema~parent.
    gv_spread_count = ( gv_spread / 100 ) * lo_priority->lines.

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

    DATA:  lv_key      TYPE string.
    LOOP AT gt_consultant_spread INTO DATA(ls_consultant_spread).

      lv_key = ls_consultant_spread-consultant.
      zif_project_estimator_schema~set_child(
                                  NEW zcl_project_schema_consultant(
                                     iv_key = lv_key
                                     io_parent = me
                                     iv_spread  = ls_consultant_spread ) ) .
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_project_estimator_schema~run_schema.

    zif_project_estimator_schema~compute( ).

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
