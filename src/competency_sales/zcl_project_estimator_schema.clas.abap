class ZCL_PROJECT_ESTIMATOR_SCHEMA definition
  public
  create public .

public section.

  interfaces ZIF_PROJECT_ESTIMATOR_ATTR .
  interfaces ZIF_PROJECT_ESTIMATOR_SCHEMA .

  methods CONSTRUCTOR
    importing
      !IT_DUMP type ref to DATA .
protected section.
private section.

  data GT_MODULES type ZIF_PROJECT_ESTIMATOR_ATTR~TT_MODULE .
  data GT_SCHEMA type ZIF_PROJECT_ESTIMATOR_ATTR~TT_SCHEMA .
  data GT_DUMP type ref to DATA .
  data KEY type CHAR20 .
  data GO_SCHEMA_STRUCT type ref to CL_ABAP_STRUCTDESCR .
  data GT_SCHEMA_OUTPUT type ref to DATA .

  methods GET_SCHEMA_STRUCT .
  methods GET_SCHEMA .
ENDCLASS.



CLASS ZCL_PROJECT_ESTIMATOR_SCHEMA IMPLEMENTATION.


  METHOD constructor.
    gt_dump = it_dump.
    zif_project_estimator_schema~schema_name = 'SCHEMA'.
    zif_project_estimator_schema~key = key = 'SCHEMA'.
  ENDMETHOD.


  METHOD get_schema.

    get_schema_struct( ).
    ASSERT go_schema_struct IS BOUND.
*   Internal table descriptor for the Schema structure
    DATA(lo_table) = cl_abap_tabledescr=>create( go_schema_struct ).
*   Create internal table with table descriptor
    CREATE DATA gt_schema_output TYPE HANDLE lo_table.

  ENDMETHOD.


  METHOD GET_SCHEMA_STRUCT.
    DATA: lt_components TYPE cl_abap_structdescr=>component_table.

    zif_project_estimator_schema~get_schema_descriptor( CHANGING ct_components = lt_components ).

    go_schema_struct = cl_abap_structdescr=>get( p_components = lt_components ).
  ENDMETHOD.


  METHOD zif_project_estimator_schema~get_children.
    rt_children = zif_project_estimator_schema~children.
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
      ls_child-child->get_schema_descriptor( changing ct_components = ct_components ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_project_estimator_schema~get_schema_output.
    get_schema( ).
    ASSERT gt_schema_output IS BOUND.

    FIELD-SYMBOLS: <lt_schema_output>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <lfs_schema_out> TYPE any,
                   <lfs_value>      TYPE any.

    ASSIGN gt_schema_output->* TO <lt_schema_output>.




    APPEND INITIAL LINE TO <lt_schema_output> ASSIGNING <lfs_schema_out> .
    ASSERT sy-subrc EQ 0.
    ASSIGN COMPONENT zif_project_estimator_schema~schema_name OF STRUCTURE <lfs_schema_out> TO <lfs_value>.


    <lfs_value> = key.
    READ TABLE zif_project_estimator_schema~children INTO DATA(ls_child) INDEX 1.
    ls_child-child->get_schema_output( EXPORTING it_schema_output = gt_schema_output
                                 IMPORTING et_schema_output = et_schema_output ).






  ENDMETHOD.


  METHOD zif_project_estimator_schema~invoke_schema.

    DATA: lt_dump_tab TYPE STANDARD TABLE OF zif_project_estimator_attr~ty_dump,
          lt_modules  TYPE zif_project_estimator_attr~tt_module.
    FIELD-SYMBOLS: <lft_dump> TYPE STANDARD TABLE.
    ASSIGN  gt_dump->* TO <lft_dump>.
    lt_dump_tab = <lft_dump>.


    zif_project_estimator_schema~set_parent( me ).

    zif_project_estimator_schema~set_child(
                      NEW zcl_project_schema_ams( it_dump = gt_dump
                                                  io_parent = me ) ).

    LOOP AT zif_project_estimator_schema~children INTO DATA(ls_child).
      ls_child-child->invoke_schema( ).
    ENDLOOP.

*   GET unique modules
    zcl_project_estimator_engine=>zif_project_estimator_engine~get_distinct( EXPORTING it_dump = lt_dump_tab
                                                         iv_field = 'MODULE'
                                                     IMPORTING et_distinct = lt_modules ).
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


  METHOD zif_project_estimator_schema~set_parent.
    zif_project_estimator_schema~parent = io_parent.
  ENDMETHOD.
ENDCLASS.
