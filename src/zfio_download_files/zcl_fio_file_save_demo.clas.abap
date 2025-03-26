CLASS zcl_fio_file_save_demo DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      convert_to_csv
        IMPORTING
          data       TYPE STANDARD TABLE
        RETURNING
          VALUE(rv_csv) TYPE string.
ENDCLASS.



CLASS zcl_fio_file_save_demo IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    TYPES: BEGIN OF sample_row,

             book_name TYPE c LENGTH 128,
             author    TYPE c LENGTH 128,
             rating    TYPE p LENGTH 2 DECIMALS 2,
           END OF sample_row.

    DATA files       TYPE zfio_tt_files.
    DATA sample_data TYPE STANDARD TABLE OF sample_row.

    sample_data = VALUE #(
        (  book_name = 'The Psychology of Money'  author = 'Morgan Housel'  rating = '4.3' )
        (  book_name = 'The Millionaire Next Door'  author = 'Thomas J. Stanley, William D. Danko'  rating = '4.04' )
        (  book_name = 'Thinking, Fast and Slow'  author = 'Daniel Kahneman'  rating = '4.17' ) ).

    DATA(csv_string) = convert_to_csv( sample_data  ).

    DATA(books_xtring) = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( csv_string ).


    files = VALUE #( ( file_mimetype = 'text/csv' file_name = 'books.csv' file_content = books_xtring ) ).
    DATA(file_save_obj) = NEW zfio_files_save_to_db( ).

    TRY.
        file_save_obj->create_files( files = files ).
        out->write( 'Records Updated Use Service Binding ZFIO_UI_FILES_OV4 to Launch the Fiori App' ).
      CATCH zcm_fio_checks INTO DATA(fio_error).

        out->write( fio_error->get_text( ) ).

    ENDTRY.
  ENDMETHOD.
  METHOD convert_to_csv.

* Warning: AI Generated code, use at your own risk


 DATA: lt_components TYPE cl_abap_structdescr=>component_table,
        lo_tabdescr   TYPE REF TO cl_abap_tabledescr,
        lo_strucdescr TYPE REF TO cl_abap_structdescr,
        lv_header     TYPE string,
        lv_value      TYPE string,
        lv_line       TYPE string.

  FIELD-SYMBOLS: <ls_data>      TYPE any,
                 <lv_value>     TYPE any,
                 <ls_component> LIKE LINE OF lt_components.

  " Get table description and its components (columns)
  lo_tabdescr ?= cl_abap_typedescr=>describe_by_data( data ).
  lo_strucdescr ?= lo_tabdescr->get_table_line_type( ).
  lt_components = lo_strucdescr->get_components( ).

  " Create header row with column names
  LOOP AT lt_components ASSIGNING <ls_component>.
    IF sy-tabix > 1.
      lv_header = lv_header && `,`.
    ENDIF.
    lv_header = lv_header && <ls_component>-name.
  ENDLOOP.

  " Add header to the result
  rv_csv = lv_header && cl_abap_char_utilities=>newline.

  " Process each row in the table
  LOOP AT data ASSIGNING <ls_data>.
    CLEAR lv_line.

    " Process each column in the row
    LOOP AT lt_components ASSIGNING <ls_component>.
      " Add comma separator between columns
      IF sy-tabix > 1.
        lv_line = lv_line && `,`.
      ENDIF.

      " Get the field value
      ASSIGN COMPONENT <ls_component>-name OF STRUCTURE <ls_data> TO <lv_value>.
      IF sy-subrc = 0.
        " Convert the value to string
        lv_value = condense( |{ <lv_value> }| ).

        " Check if value contains special characters that need quoting
        IF lv_value CS ',' OR lv_value CS '"' OR lv_value CS cl_abap_char_utilities=>newline.
          " Escape any double quotes by doubling them
          REPLACE ALL OCCURRENCES OF '"' IN lv_value WITH '""'.
          " Enclose in double quotes
          lv_value = |"{ lv_value }"|.
        ENDIF.

        lv_line = lv_line && lv_value.
      ENDIF.
    ENDLOOP.

    " Add the row to the result
    rv_csv = rv_csv && lv_line && cl_abap_char_utilities=>newline.
  ENDLOOP.

  " Remove the last newline
  IF strlen( rv_csv ) >= strlen( cl_abap_char_utilities=>newline ).
    rv_csv = substring( val = rv_csv off = 0 len = strlen( rv_csv ) - strlen( cl_abap_char_utilities=>newline ) ).
  ENDIF.


  ENDMETHOD.

ENDCLASS.
