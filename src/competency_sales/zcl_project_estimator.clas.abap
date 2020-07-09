class ZCL_PROJECT_ESTIMATOR definition
  public
  abstract
  create public .

public section.

  interfaces ZIF_PROJECT_ESTIMATOR_ADMIN .
  interfaces ZIF_PROJECT_ESTIMATOR_ATTR .
  interfaces ZIF_TOOLS_ESTIMATOR_OUTPUT .
  interfaces ZIF_PROJECT_ESTIMATOR .
  interfaces ZIF_PROJECT_ESTIMATOR_ENGINE .
  interfaces ZIF_PROJECT_ESTIMATOR_INPUT .
  interfaces ZIF_PROJECT_ESTIMATOR_RESOURCE .
  interfaces ZIF_PROJECT_ESTIMATOR_SKILL .

  data GT_DUMP type ref to DATA .

  events PROCESS_DUMP
    exporting
      value(IO_ESTIMATOR) type ref to ZCL_PROJECT_ESTIMATOR .

  methods CONSTRUCTOR .
protected section.

  data GO_DB type ref to ZIF_PROJECT_ESTIMATOR_DB .
  data GO_SCHEMA type ref to ZCL_PROJECT_ESTIMATOR_SCHEMA .
  data GT_MODULE_SCHEMA type ZIF_PROJECT_ESTIMATOR_ATTR~TT_SCHEMA .
  data GT_MODULES type ZIF_PROJECT_ESTIMATOR_ATTR~TT_MODULE .
  data GO_ENGINE type ref to ZIF_PROJECT_ESTIMATOR_ENGINE .

  methods GET_DUMP
    exporting
      !ET_DUMP type ref to DATA .
private section.

  data GT_PRIORITY type ZIF_PROJECT_ESTIMATOR_ATTR~TT_PRIORITY .
  data GT_DATA type ZIF_PROJECT_ESTIMATOR_ATTR~TT_DUMP .

  methods GET_TEMPLATE_DESCRIPTOR
    importing
      !IV_TEMPLATE_NAME type CHAR12
    returning
      value(RO_STRUCT) type ref to CL_ABAP_STRUCTDESCR .
ENDCLASS.



CLASS ZCL_PROJECT_ESTIMATOR IMPLEMENTATION.


  METHOD constructor.
    CREATE OBJECT go_db TYPE zcl_project_estimator_db.
  ENDMETHOD.


  method GET_DUMP.
  et_dump = gt_dump.
  endmethod.


  METHOD get_template_descriptor.
    DATA:
      lt_components TYPE cl_abap_structdescr=>component_table,
      lt_definition TYPE STANDARD TABLE OF zif_project_estimator_attr~ty_meta,
      lo_input      TYPE REF TO zcl_abapgit_xml_input.

*   Set Key
    zif_project_estimator_attr~gs_project_key-project = zif_project_estimator_attr~g_project.
    zif_project_estimator_attr~gs_project_key-type = 'TEMPLATE'.
    zif_project_estimator_attr~gs_project_key-value = iv_template_name.
*   Set key of RLS to acces the DB
    go_db->set_key( iv_key  = zif_project_estimator_attr~gs_project_key ).
*   Get Key Data - RLS template XML data
    DATA(ls_key_data) = go_db->read_key_data( ).
    DATA(lv_xml) = ls_key_data-data.
*   Create XML transformation instance
    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.
*   Read the defintion for RLS download template from the XML
    lo_input->read( EXPORTING iv_name = iv_template_name
                CHANGING cg_data = lt_definition ).
*   Build structure defintion for the RLS components
    LOOP AT lt_definition INTO DATA(ls_defintion).
      APPEND INITIAL LINE TO lt_components ASSIGNING FIELD-SYMBOL(<lfs_component>).
*     Component name
      <lfs_component>-name = ls_defintion-name.
*     Component type
      <lfs_component>-type ?= cl_abap_datadescr=>describe_by_name( ls_defintion-absolute_name ).
    ENDLOOP.
*   Structure descriptor for RLS template
    ro_struct = cl_abap_structdescr=>get( p_components = lt_components ).
  ENDMETHOD.


METHOD zif_project_estimator_admin~set_schema.

  DATA : lv_return            TYPE i,
         lt_consultant_spread TYPE zif_project_estimator_attr~tt_consultant_spread,
         ls_consultant_spread TYPE zif_project_estimator_attr~ty_consultant_spread,
         lt_spread            TYPE zif_project_estimator_attr~tt_level_spread,
         ls_spread            TYPE zif_project_estimator_attr~ty_level_spread,
         lt_filetable         TYPE filetable,
         lo_input             TYPE REF TO zcl_abapgit_xml_input,
         lv_filename          TYPE string,
         lt_datatab           TYPE STANDARD TABLE OF string,
         ls_textid            TYPE scx_t100key.

* Set key
  zif_project_estimator_attr~gs_project_key-project = zif_project_estimator_attr~g_project.
  zif_project_estimator_attr~gs_project_key-type    = 'SCHEMA'.
  zif_project_estimator_attr~gs_project_key-value   = iv_schema_name.

  go_db->set_key( iv_key = zif_project_estimator_attr~gs_project_key ).

* Set Dialog Title

  CONCATENATE 'Choose your' zif_project_estimator_attr~g_project
               iv_schema_name 'Schema' INTO DATA(lv_title) SEPARATED BY space.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      window_title            = lv_title
      default_extension       = 'XML'
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_return
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4 ).
  IF sy-subrc EQ 0.
    READ TABLE lt_filetable INTO DATA(ls_file) INDEX 1.
    lv_filename = ls_file.
  ELSE.

* Raise Exception

    ls_textid-msgid = '00' . ls_textid-msgno = '000'.
    ls_textid-attr1 = ' Error in open dialog'.

    RAISE EXCEPTION TYPE zcx_project_estimator
      EXPORTING
        textid = ls_textid
        msgty  = 'E'.

  ENDIF.

* FILE UPLOAD

  CALL METHOD cl_gui_frontend_services=>gui_upload(
    EXPORTING
      filename                = lv_filename
    IMPORTING
      filelength              = DATA(lv_filelength)
      header                  = DATA(lv_header)
    CHANGING
      data_tab                = lt_datatab
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      OTHERS                  = 9 ).

  IF sy-subrc EQ 0.

    LOOP AT lt_datatab INTO DATA(ls_data).
      AT FIRST.
        DATA(lv_xml)  = ls_data.
        CONTINUE.
      ENDAT.
      CONCATENATE lv_xml cl_abap_char_utilities=>newline ls_data INTO lv_xml.
    ENDLOOP.

  ELSE.

* Raise Exception

    ls_textid-msgid = '00' . ls_textid-msgno = '000'.
    ls_textid-attr1 = ' Error in open dialog'.

    RAISE EXCEPTION TYPE zcx_project_estimator
      EXPORTING
        textid = ls_textid
        msgty  = 'E'.

  ENDIF.

* Checking the record data and its definition

  CREATE OBJECT lo_input
    EXPORTING
      iv_xml = lv_xml.

  lo_input->read( EXPORTING iv_name = 'CONSULTANT'
              CHANGING cg_data = lt_consultant_spread ).

  lo_input->read( EXPORTING iv_name = 'LEVEL'
              CHANGING cg_data = lt_spread ).

  IF  lt_consultant_spread IS NOT INITIAL
  AND lt_spread IS NOT INITIAL.
    TRY.
        DATA(ls_key_data) = go_db->read_key_data( ).
      CATCH zcx_project_estimator INTO DATA(lcx_root).
*       First time upload
        IF ls_key_data IS INITIAL.
          IF iv_schema_name EQ 'DEFAULT'
          OR iv_schema_name EQ 'ENU'
          OR iv_schema_name EQ 'MFG'
          OR iv_schema_name EQ 'HLS'
          OR iv_schema_name EQ 'BFSI'.
            DATA(ls_key) = go_db->get_key( ).
            ls_key_data  = ls_key.
          ELSE.
            ls_textid-msgid = '00' . ls_textid-msgno = '000'.
            ls_textid-attr1 = ' Invalid Schema Name'.

            RAISE EXCEPTION TYPE zcx_project_estimator
              EXPORTING
                textid = ls_textid
                msgty  = 'E'.
          ENDIF.

        ENDIF.
    ENDTRY.

*   Update the Schema xml
    ls_key_data-data = lv_xml.
    go_db->update_key_data( iv_key_record = ls_key_data ).
    go_db->update_db( ).

  ENDIF.


ENDMETHOD.


  METHOD zif_project_estimator_admin~set_template.


    DATA: lv_return     TYPE i,
          lt_definition TYPE STANDARD TABLE OF zif_project_estimator_attr~ty_meta,
          lt_filetable  TYPE filetable,
          lo_input      TYPE REF TO zcl_abapgit_xml_input,
          lv_filename   TYPE string,
          lt_datatab    TYPE STANDARD TABLE OF string,
          ls_textid     TYPE scx_t100key.

*   Set Key
    zif_project_estimator_attr~gs_project_key-project = zif_project_estimator_attr~g_project.
    zif_project_estimator_attr~gs_project_key-type = 'TEMPLATE'.
    zif_project_estimator_attr~gs_project_key-value = iv_template_name.

    go_db->set_key( iv_key  = zif_project_estimator_attr~gs_project_key ).
*   Set Dialog title
    CONCATENATE 'Choose Your' zif_project_estimator_attr~g_project
                iv_template_name 'Template' INTO DATA(lv_title).

    CALL METHOD cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = lv_title
        default_extension       = 'XML'
      CHANGING
        file_table              = lt_filetable
        rc                      = lv_return
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4 ).
    IF sy-subrc EQ 0.
      READ TABLE lt_filetable INTO DATA(ls_file) INDEX 1.
      lv_filename = ls_file.
    ELSE.
*  Raise Exception
      ls_textid-msgid = '00'. ls_textid-msgno = '000'.
      ls_textid-attr1 = 'Error in Open dialog'.

      RAISE EXCEPTION TYPE zcx_project_estimator
        EXPORTING
          textid = ls_textid
          msgty  = 'E'.

    ENDIF.

* File Upload
    CALL METHOD cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = lv_filename
      IMPORTING
        filelength              = DATA(lv_filelength)
        header                  = DATA(lv_header)
      CHANGING
        data_tab                = lt_datatab
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        OTHERS                  = 9 ).
    IF sy-subrc EQ 0.

      LOOP AT lt_datatab INTO DATA(ls_data).
        AT FIRST.
          DATA(lv_xml)  = ls_data.
          CONTINUE.
        ENDAT.
        CONCATENATE lv_xml cl_abap_char_utilities=>newline ls_data INTO lv_xml.
      ENDLOOP.
    ELSE.
*     Raise exception
ls_textid-msgid = '00'. ls_textid-msgno = '000'.
      ls_textid-attr1 = 'File Open Error'.

      RAISE EXCEPTION TYPE zcx_project_estimator
        EXPORTING
          textid = ls_textid
          msgty  = 'E'.


    ENDIF.
*Checking the record data  and its definition

    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.

    lo_input->read( EXPORTING iv_name = iv_template_name
                CHANGING cg_data = lt_definition ).






    IF lt_definition IS NOT INITIAL.
      TRY.
          DATA(ls_key_data) = go_db->read_key_data( ).

        CATCH zcx_project_estimator INTO DATA(lcx_root).
*         First time upload
          IF ls_key_data IS INITIAL.
            IF iv_template_name = zif_project_estimator_attr~c_template_upload "Upload Template
            OR iv_template_name = zif_project_estimator_attr~c_template_rls.   "RLS Template
              DATA(ls_key) = go_db->get_key( ).
              ls_key_data = ls_key.
            ELSE.
*           Raise exception.
       DATA(ls_read_key_data) = 'Unable to read'.
            RAISE EXCEPTION TYPE zcx_project_estimator.

            ENDIF.
          ENDIF.
      ENDTRY.
*     Modification
      ls_key_data-data = lv_xml.
      go_db->update_key_data( iv_key_record = ls_key_data ).
      go_db->update_db( ).

    ENDIF.
  ENDMETHOD.


  METHOD zif_project_estimator_engine~handle_action.
    CASE iv_action.
      WHEN 'LOAD_PROJECT'.
        go_db->load_project( ).
    ENDCASE.
  ENDMETHOD.


  METHOD zif_project_estimator_engine~process_dump.
*    DATA: lt_dump TYPE STANDARD TABLE OF zif_project_estimator_attr~ty_dump.
*    FIELD-SYMBOLS: <lft_dump> TYPE STANDARD TABLE.
*    ASSIGN  gt_dump->* TO <lft_dump>.
*    lt_dump = <lft_dump>.
*
*    LOOP AT <lft_dump> INTO field-symbol(<lfs_dump>).
*      <lfs_dump>-module = 'MM'.
*    ENDLOOP.
*assign gt_dump->* to <lft_dump>.

**   Get Unique modules
*    zif_project_estimator_engine~get_distinct( EXPORTING it_dump = gt_dump
*                                                         iv_field = 'MODULE'
*                                                     IMPORTING ET_DISTINCT = gt_modules ).
**   get unique priorities
*    zif_project_estimator_engine~get_distinct( EXPORTING it_dump = gt_dump
*                                                         iv_field = 'PRIORITY'
*                                                     IMPORTING ET_DISTINCT = gt_priority ).



*  Get Months
*  Get Schema
*  Get ticket counts for Moudle/Priority
*  Compute spread for Module priority
*  Compute Efforts for Spread
*  Compute FTE for Efforts
  ENDMETHOD.


  METHOD zif_project_estimator_input~load_dump.
    TYPES: BEGIN OF ty_data,
             reported_date TYPE char20,
             reported_time TYPE char20,
             time_zone     TYPE char20,
             module        TYPE char20,
             priority      TYPE char20,
             complexity    TYPE char20,
             start_date    TYPE char20,
             start_time    TYPE char20,
             end_date      TYPE char20,
             end_time      TYPE char20,
           END OF ty_data.


    DATA: lv_return     TYPE i,
          lt_definition TYPE STANDARD TABLE OF zif_project_estimator_attr~ty_meta,
          lt_filetable  TYPE filetable,
          lo_input      TYPE REF TO zcl_abapgit_xml_input,
          lv_filename   TYPE string,
          lt_datatab    TYPE STANDARD TABLE OF ty_data,
          ls_data       TYPE ty_data,
          ls_textid     TYPE scx_t100key.
    DATA: lt_dump TYPE STANDARD TABLE OF string.

    CALL METHOD cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = 'Select dump file'
        default_extension       = 'XML'
      CHANGING
        file_table              = lt_filetable
        rc                      = lv_return
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4 ).
    IF sy-subrc EQ 0.
      READ TABLE lt_filetable INTO DATA(ls_file) INDEX 1.
      lv_filename = ls_file.
    ELSE.

      ls_textid-msgid = '00'. ls_textid-msgno = '000'.
      ls_textid-attr1 = 'Error in Open dialog'.

      RAISE EXCEPTION TYPE zcx_project_estimator
        EXPORTING
          textid = ls_textid
          msgty  = 'E'.

    ENDIF.
    DATA:  file TYPE localfile.
    file = ls_file.
*    CALL FUNCTION 'UPLOAD_XLS_FILE_2_ITAB'
*      EXPORTING
*        i_filename = file
*      TABLES
*        e_itab     = lt_dump
**     EXCEPTIONS
**       FILE_ERROR = 1
**       OTHERS     =
*      .
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
*    TRY.
*        DATA(mv_file_data) = cl_openxml_helper=>load_local_file( lv_filename ).
*      CATCH cx_openxml_not_found.
*    ENDTRY.
*
*
*    TRY.
*        DATA(mo_package) = cl_xlsx_document=>load_document( iv_data  =  mv_file_data ).
*      CATCH cx_openxml_format cx_openxml_not_found.
*    ENDTRY.
*    TRY.
*        DATA(mo_parts) = mo_package->get_parts( ).
*      CATCH cx_openxml_format cx_openxml_not_found.
*    ENDTRY.
*
*    DATA lo_xml_part TYPE REF TO cl_openxml_part.
*    DATA lo_xml_part_uri TYPE REF TO cl_openxml_parturi.
*    DATA lv_uri TYPE string.
*    TRY.
*        lv_uri = mo_parts->get_part( 2 )->get_parts( )->get_part( 3 )->get_uri( )->get_uri( ).
*        lo_xml_part_uri = cl_openxml_parturi=>create_from_partname( lv_uri ).
*        lo_xml_part = mo_package->get_part_by_uri( lo_xml_part_uri ).
*        DATA(xml_data) = lo_xml_part->get_data( ).
*      CATCH cx_openxml_format  cx_openxml_not_found.
*    ENDTRY.



    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename            = lv_filename
        filetype            = 'ASC'
        has_field_separator = 'X'
*       HEADER_LENGTH       = 0
*       READ_BY_LINE        = 'X'
*       DAT_MODE            = ' '
*       CODEPAGE            = ' '
*       IGNORE_CERR         = ABAP_TRUE
*       REPLACEMENT         = '#'
*       CHECK_BOM           = ' '
*       VIRUS_SCAN_PROFILE  =
*       NO_AUTH_CHECK       = ' '
* IMPORTING
*       FILELENGTH          =
*       HEADER              =
      TABLES
        data_tab            = lt_dump
* CHANGING
*       ISSCANPERFORMED     = ' '
* EXCEPTIONS
*       FILE_OPEN_ERROR     = 1
*       FILE_READ_ERROR     = 2
*       NO_BATCH            = 3
*       GUI_REFUSE_FILETRANSFER       = 4
*       INVALID_TYPE        = 5
*       NO_AUTHORITY        = 6
*       UNKNOWN_ERROR       = 7
*       BAD_DATA_FORMAT     = 8
*       HEADER_NOT_ALLOWED  = 9
*       SEPARATOR_NOT_ALLOWED         = 10
*       HEADER_TOO_LONG     = 11
*       UNKNOWN_DP_ERROR    = 12
*       ACCESS_DENIED       = 13
*       DP_OUT_OF_MEMORY    = 14
*       DISK_FULL           = 15
*       DP_TIMEOUT          = 16
*       OTHERS              = 17
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    DATA(lr_csv) = cl_rsda_csv_converter=>create( ).

    LOOP AT lt_dump INTO DATA(ls_dump) FROM 2.
      CLEAR: ls_data.
      CALL METHOD lr_csv->csv_to_structure
        EXPORTING
          i_data   = ls_dump
        IMPORTING
          e_s_data = ls_data.
      APPEND ls_data TO lt_datatab.
      AT LAST.
        gt_data = lt_datatab.
        GET REFERENCE OF gt_data INTO gt_dump.
*        raise event process_dump. ( it_dump = gt_dump ).
*        go_engine = NEW zcl_project_estimator_engine( it_dump = gt_dump ).
*        go_engine->process_dump( ).
      ENDAT.

    ENDLOOP.

    IF sy-subrc EQ 0.

*      LOOP AT lt_datatab INTO DATA(ls_data).
*        AT FIRST.
*          DATA(lv_xml)  = ls_data.
*          CONTINUE.
*        ENDAT.
*        CONCATENATE lv_xml cl_abap_char_utilities=>newline ls_data INTO lv_xml.
*      ENDLOOP.
    ELSE.
*     Raise exception

    ENDIF.

  ENDMETHOD.


  METHOD zif_project_estimator_skill~determine_skills.
  ENDMETHOD.


  METHOD zif_tools_estimator_output~get_template.


    DATA:
      lt_components   TYPE cl_abap_structdescr=>component_table,
      lt_definition   TYPE STANDARD TABLE OF zif_project_estimator_attr~ty_meta,
      lo_input        TYPE REF TO zcl_abapgit_xml_input,
      lv_default_name TYPE string,
      lv_filename     TYPE string,
      lv_path         TYPE string,
      lv_fullpath     TYPE string,
      lt_datatab      TYPE STANDARD TABLE OF string,
      ls_textid       TYPE scx_t100key,
      lt_rls_template TYPE REF TO data.
    FIELD-SYMBOLS: <lt_rls_table>     TYPE STANDARD TABLE.

*   Structure descriptor for RLS template
    DATA(lo_struct) = get_template_descriptor( iv_template_name = iv_template_name ).

*   Internal table descriptor for the RLS structure
    DATA(lo_table) = cl_abap_tabledescr=>create( lo_struct ).
*   Create internal table with table descriptor
    CREATE DATA lt_rls_template TYPE HANDLE lo_table.
    ASSIGN lt_rls_template->* TO <lt_rls_table>.

    ASSERT sy-subrc EQ 0.
*   Fill dummy data for download
    APPEND INITIAL LINE TO <lt_rls_table> ASSIGNING FIELD-SYMBOL(<lfs_rls>).
    <lfs_rls> = 'AMS Template reference     AMS Template reference    AMS Template reference'.

    CONCATENATE zif_project_estimator_attr~g_project
      iv_template_name 'Template.CSV' INTO lv_default_name SEPARATED BY '_'.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
*       window_title      = ' '
        default_extension = 'CSV'
        default_file_name = lv_default_name
*       initial_directory = 'c:\temp\'
      CHANGING
        filename          = lv_filename
        path              = lv_path
        fullpath          = lv_fullpath.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename              = lv_fullpath
        write_field_separator = 'X'
        FIELDNAMES            = lt_definition
        replacement           = ','
      CHANGING
        data_tab              = <lt_rls_table>.
  ENDMETHOD.
ENDCLASS.
