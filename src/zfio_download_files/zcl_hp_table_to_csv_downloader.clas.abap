CLASS zcl_hp_table_to_csv_downloader DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    METHODS constructor.

    methods download_table_as_xstring
    IMPORTING
        iv_table_name   TYPE string
      EXPORTING
        ev_xtring TYPE  XSTRING
        ev_file_name    TYPE string
      RAISING
        cx_abap_invalid_name
        cx_sy_open_sql_db.

    METHODS download_table_as_csv
      IMPORTING
        iv_table_name   TYPE string
      EXPORTING
        ev_file_content TYPE string
        ev_file_name    TYPE string
      RAISING
        cx_abap_invalid_name
        cx_sy_open_sql_db.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_table_columns
      IMPORTING
        iv_table_name     TYPE string
      RETURNING
        VALUE(rt_columns) TYPE string_table
      RAISING
        cx_abap_invalid_name.

    METHODS build_csv_content
      IMPORTING
        it_data       TYPE REF TO data
        it_columns    TYPE string_table
      RETURNING
        VALUE(rv_csv) TYPE string.

    METHODS escape_csv_field
      IMPORTING
        iv_field          TYPE string
      RETURNING
        VALUE(rv_escaped) TYPE string.


ENDCLASS.



CLASS zcl_hp_table_to_csv_downloader IMPLEMENTATION.

  METHOD constructor.
    " Constructor method
  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

  try.
  download_table_as_xstring(
    EXPORTING
      iv_table_name =  'ZHP_COSTS_A'
    IMPORTING
      ev_xtring     = data(xstring)
      ev_file_name  = data(file_name)
  ).

  out->write(  |'File Name' { file_name } | ).

  out->write(  |'XString' { xstring } | ).

    out->write(  |'XString length' { xstrlen( xstring )  } 'bytes' | ).

  CATCH cx_abap_invalid_name.
  CATCH cx_sy_open_sql_db.

 ENDTRY.
*    DATA:
*      lv_table_name   TYPE string,
*      lv_file_content TYPE string,
*      lv_file_name    TYPE string.
*
*    TRY.
*        " Get table name from user input or use a default one
**        lv_table_name = COND #( WHEN out->get_user_input( 'Enter table name:' ) IS NOT INITIAL
**                                THEN io_out->get_user_input( 'Enter table name:' )
**                                ELSE 'SCARR' ). " Default table
*
**        lv_table_name = 'ZHP_FLOORS_A'.
*
*        lv_table_name = 'ZHP_HOUSE_DATA_A'.
*
*        " Download table content
*        download_table_as_csv(
*          EXPORTING
*            iv_table_name   = lv_table_name
*          IMPORTING
*            ev_file_content = lv_file_content
*            ev_file_name    = lv_file_name
*        ).
*
*
*        " Display success message and preview
*        out->write( |Table { lv_table_name } exported to CSV successfully.| ).
*        out->write( |File name: { lv_file_name }| ).
*        out->write( |Preview of CSV content:| ).
*        out->write( substring( val = lv_file_content off = 0 len = 500 ) ). " Show first 500 chars
*
*
*
*      DATA(xstring) = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( lv_file_content ).
*
*
*      CATCH cx_root INTO DATA(lx_exception).
*        out->write( |Error: { lx_exception->get_text( ) }| ).
*    ENDTRY.
*
*    DATA:
*      lv_filename  TYPE string,
*      lv_mime_type TYPE string VALUE 'text/csv',
*      lv_xstring   TYPE xstring.
*
*    lv_xstring = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( lv_file_content ).
*    " Define the filename (ensure it has .csv extension)
*    lv_filename = lv_file_name.


  ENDMETHOD.

  METHOD download_table_as_csv.
    DATA:
      lt_columns  TYPE string_table,
      lr_tabdescr TYPE REF TO cl_abap_tabledescr,
      lr_data     TYPE REF TO data.

    " Generate file name
    ev_file_name = |{ iv_table_name }_{ sy-datum }_{ sy-uzeit }.csv|.

    " Get table columns
    lt_columns = get_table_columns( iv_table_name ).

    " Create a dynamic internal table based on the table name
    TRY.
        " Create dynamic table type
        DATA(lr_typedescr) = CAST cl_abap_structdescr(
          cl_abap_typedescr=>describe_by_name( iv_table_name ) ).
        lr_tabdescr = cl_abap_tabledescr=>create( lr_typedescr ).

        " Create dynamic table instance
        CREATE DATA lr_data TYPE HANDLE lr_tabdescr.
        FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.
        ASSIGN lr_data->* TO <lt_table>.

        " Select data from the table
        SELECT * FROM (iv_table_name) INTO TABLE @<lt_table>
          UP TO 10000 ROWS. " Limit rows for performance

        " Build CSV content
        ev_file_content = build_csv_content(
          it_data    = lr_data
          it_columns = lt_columns
        ).

      CATCH cx_sy_create_data_error INTO DATA(lx_data_error).
        RAISE EXCEPTION TYPE cx_abap_invalid_name
          EXPORTING
            textid = |Error creating data reference: { lx_data_error->get_text( ) }|.
    ENDTRY.
  ENDMETHOD.

  METHOD get_table_columns.
    DATA:
      lr_table_descr TYPE REF TO cl_abap_structdescr,
      lt_components  TYPE cl_abap_structdescr=>component_table.

    TRY.
        " Get table structure description
        lr_table_descr ?= cl_abap_typedescr=>describe_by_name( iv_table_name ).
        lt_components = lr_table_descr->get_components( ).

        " Extract column names
        LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_component>).
          APPEND <ls_component>-name TO rt_columns.
        ENDLOOP.

      CATCH cx_sy_move_cast_error INTO DATA(lx_cast_error).
        RAISE EXCEPTION TYPE cx_abap_invalid_name
          EXPORTING
            textid = |Error getting table structure: { lx_cast_error->get_text( ) }|.
    ENDTRY.
  ENDMETHOD.

  METHOD build_csv_content.
    DATA:
      lv_line      TYPE string,
      lt_lines     TYPE string_table,
      lv_data_line TYPE string.

    FIELD-SYMBOLS:
      <lt_table> TYPE ANY TABLE,
      <ls_row>   TYPE any,
      <lv_field> TYPE any.

    " Add header line with column names
    LOOP AT it_columns INTO DATA(lv_column).
      IF sy-tabix > 1.
        lv_line = lv_line && ','.
      ENDIF.
      lv_line = lv_line && escape_csv_field( lv_column ).
    ENDLOOP.
    APPEND lv_line TO lt_lines.

    " Process data rows
    ASSIGN it_data->* TO <lt_table>.
    LOOP AT <lt_table> ASSIGNING <ls_row>.
      CLEAR lv_line.

      LOOP AT it_columns INTO lv_column.
        IF sy-tabix > 1.
          lv_line = lv_line && ','.
        ENDIF.

        ASSIGN COMPONENT lv_column OF STRUCTURE <ls_row> TO <lv_field>.
        IF sy-subrc = 0.
          " Convert any data type to string
          lv_data_line = CONV string( <lv_field> ).
          lv_line = lv_line && escape_csv_field( lv_data_line ).
        ELSE.
          " Handle missing fields
          lv_line = lv_line && ''.
        ENDIF.
      ENDLOOP.

      APPEND lv_line TO lt_lines.
    ENDLOOP.

    " Combine all lines
    rv_csv = concat_lines_of( table = lt_lines sep = cl_abap_char_utilities=>newline ).
  ENDMETHOD.

  METHOD escape_csv_field.
    " CSV escaping rules:
    " - Enclose in double quotes if field contains comma, quote or newline
    " - Double any quotes within the field

    " Check if field needs escaping
    IF iv_field CS ',' OR iv_field CS '"' OR iv_field CS cl_abap_char_utilities=>newline.
      " Replace double quotes with double double quotes
      rv_escaped = replace( val = iv_field sub = '"' with = '""' occ = 0 ).
      " Enclose in double quotes
      rv_escaped = |"{ rv_escaped }"|.
    ELSE.
      rv_escaped = iv_field.
    ENDIF.
  ENDMETHOD.

  METHOD download_table_as_xstring.
    " TODO: parameter EV_FILE_CONTENT is never cleared or assigned (ABAP cleaner)
    " TODO: parameter EV_FILE_NAME is never cleared or assigned (ABAP cleaner)

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_file_content TYPE string.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_file_name    TYPE string.


    download_table_as_csv( EXPORTING iv_table_name   = iv_table_name
                               IMPORTING ev_file_content = lv_file_content
                                         ev_file_name    = lv_file_name ).

    ev_xtring = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( lv_file_content ).
    ev_file_name = lv_file_name.

  ENDMETHOD.

ENDCLASS.
