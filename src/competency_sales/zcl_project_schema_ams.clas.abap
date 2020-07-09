class ZCL_PROJECT_SCHEMA_AMS definition
  public
  inheriting from ZCL_PROJECT_ESTIMATOR_SCHEMA
  create public .

public section.

  data GT_PRIORITY type ZIF_PROJECT_ESTIMATOR_ATTR~TT_PRIORITY .
  data GT_RECORDS type ZIF_PROJECT_ESTIMATOR_ATTR~TT_PRIORITY .

  methods CONSTRUCTOR
    importing
      !IT_DUMP type ref to DATA
      !IO_PARENT type ref to ZCL_PROJECT_ESTIMATOR_SCHEMA .

  methods ZIF_PROJECT_ESTIMATOR_SCHEMA~INVOKE_SCHEMA
    redefinition .
  methods ZIF_PROJECT_ESTIMATOR_SCHEMA~GET_SCHEMA_OUTPUT
    redefinition .
protected section.
private section.

  data GT_DUMP type ref to DATA .
  data GT_CONSULTANT_SPREAD type ZIF_PROJECT_ESTIMATOR_ATTR~TT_CONSULTANT_SPREAD .
  data KEY type CHAR20 .
ENDCLASS.



CLASS ZCL_PROJECT_SCHEMA_AMS IMPLEMENTATION.


  METHOD constructor.
    super->constructor( it_dump ).


    gt_dump = it_dump.
    zif_project_estimator_schema~parent ?= io_parent.
    zif_project_estimator_schema~schema_name = 'PROJECT'.
    zif_project_estimator_schema~key = key = zif_project_estimator_attr~c_project_ams.

*   This should be instantiated from Schema
    DATA: lt_consultant_spread TYPE zif_project_estimator_attr~tt_consultant_spread,
          ls_consultant_spread TYPE zif_project_estimator_attr~ty_consultant_spread,
          lo_input             TYPE REF TO zcl_abapgit_xml_input,
          lv_xml               TYPE string.


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
      lo_input->read( EXPORTING iv_name = 'CONSULTANT'
                  CHANGING cg_data = gt_consultant_spread ).

    ENDIF.
*   Use hardcoded values if the schema is not maintained for this project
*    CHECK gt_consultant_spread IS INITIAL.
**   MM
*    ls_consultant_spread-module = 'MM'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '2'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'MM'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '2.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'MM'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'MM'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1.2'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'MM'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'MM'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.15'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'MM'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.07'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'MM'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'C1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
**  ABAP
*    ls_consultant_spread-module = 'ABAP'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'ABAP'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '2'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'ABAP'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'ABAP'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'ABAP'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'ABAP'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'ABAP'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.25'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'ABAP'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'C1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
**FI
*    ls_consultant_spread-module = 'FI'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'FI'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'FI'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'FI'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'FI'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.25'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'FI'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'FI'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.25'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'FI'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'C1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
**HR
*
*    ls_consultant_spread-module = 'HR'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'HR'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '1.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'HR'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'HR'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'HR'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.25'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'HR'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'HR'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.25'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'HR'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'C1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
** SD
*
*    ls_consultant_spread-module = 'SD'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'SD'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '1.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'SD'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'SD'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'SD'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.25'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'SD'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'SD'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.25'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'SD'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'C1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
** QM
*
*    ls_consultant_spread-module = 'QM'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'QM'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '1.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'QM'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'QM'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'QM'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.25'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'QM'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'QM'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.25'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'QM'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'C1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
** PP
*
*    ls_consultant_spread-module = 'PP'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'PP'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '1.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'PP'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'PP'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'PP'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.25'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'PP'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'PP'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.25'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'PP'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'C1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
** CRM
*
*    ls_consultant_spread-module = 'CRM'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'CRM'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '1.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'CRM'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'CRM'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'CRM'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.25'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'CRM'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'CRM'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.25'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'CRM'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'C1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
** SRM
*
*    ls_consultant_spread-module = 'SRM'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'SRM'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '1.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'SRM'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'SRM'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'SRM'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.25'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'SRM'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'SRM'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.25'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'SRM'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'C1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
** APO
*
*    ls_consultant_spread-module = 'APO'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'APO'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'APO'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'APO'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'APO'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.25'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'APO'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'APO'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.25'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'APO'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'C1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
** BASIS
*
*    ls_consultant_spread-module = 'BASIS'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'BASIS'.
*    ls_consultant_spread-level  = 'L1'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '2'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'BASIS'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B1'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'BASIS'.
*    ls_consultant_spread-level  = 'L2'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '2'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'BASIS'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B2'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'BASIS'.
*    ls_consultant_spread-level  = 'L3'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '1'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'BASIS'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'B3'.
*    ls_consultant_spread-spread = 25.
*    ls_consultant_spread-productivity = '.25'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    ls_consultant_spread-module = 'BASIS'.
*    ls_consultant_spread-level  = 'L4'.
*    ls_consultant_spread-consultant = 'C1'.
*    ls_consultant_spread-spread = 75.
*    ls_consultant_spread-productivity = '.5'.
*    APPEND ls_consultant_spread TO lt_consultant_spread.
*
*    gt_consultant_spread = lt_consultant_spread.


  ENDMETHOD.


  METHOD zif_project_estimator_schema~get_schema_output.
*CALL METHOD SUPER->ZIF_PROJECT_ESTIMATOR_SCHEMA~GET_SCHEMA_OUTPUT
*  EXPORTING
*    IT_SCHEMA_OUTPUT =
**  IMPORTING
**    et_schema_output =
*    .


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
*CALL METHOD SUPER->ZIF_PROJECT_ESTIMATOR_SCHEMA~INVOKE_SCHEMA
*    .

    DATA: lt_dump_tab TYPE STANDARD TABLE OF zif_project_estimator_attr~ty_dump,
          lt_modules  TYPE zif_project_estimator_attr~tt_module,
          lv_key      TYPE string.
    FIELD-SYMBOLS: <lft_dump> TYPE STANDARD TABLE.
    ASSIGN  gt_dump->* TO <lft_dump>.
    lt_dump_tab = <lft_dump>.

*   GET unique modules
    zcl_project_estimator_engine=>zif_project_estimator_engine~get_distinct( EXPORTING it_dump = lt_dump_tab
                                                         iv_field = 'MODULE'
                                                     IMPORTING et_distinct = lt_modules ).
    LOOP AT lt_modules INTO DATA(ls_modules).
      lv_key = ls_modules.
      zif_project_estimator_schema~set_child(
                             NEW zcl_project_schema_modules( iv_key = lv_key
                                                             io_parent = me
                                                             it_dump = gt_dump
                                                             it_consultant_spread = gt_consultant_spread ) ).
    ENDLOOP.

    LOOP AT zif_project_estimator_schema~children INTO DATA(ls_child).
      ls_child-child->invoke_schema( ).
    ENDLOOP.


  ENDMETHOD.
ENDCLASS.
