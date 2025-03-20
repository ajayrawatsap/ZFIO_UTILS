CLASS zcl_fio_file_save_demo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      convert_to_csv
        IMPORTING
          it_data       TYPE STANDARD TABLE
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

    DATA: file_save_obj TYPE REF TO zfio_files_save_to_db,
          files         TYPE zfio_tt_files,
          sample_data   TYPE STANDARD TABLE OF sample_row.

    file_save_obj = NEW #( ).

    TRY.
        file_save_obj->create_files( files = files ).
      CATCH zcm_fio_checks INTO DATA(error).

        out->write( error->get_text(  ) ).

    ENDTRY.

    files = VALUE #( ( file_mimetype = ' ' file_name = 'test' file_content = 'ABCD' ) ).
    TRY.
        file_save_obj->create_files( files = files ).
      CATCH zcm_fio_checks INTO error.

        out->write( error->get_text(  ) ).

    ENDTRY.


    files = VALUE #( ( file_mimetype = 'text' file_name = 'test' file_content = '' ) ).

    TRY.
        file_save_obj->create_files( files = files ).
      CATCH zcm_fio_checks INTO error.

        out->write( error->get_text(  ) ).
    ENDTRY.

    files = VALUE #( ( file_mimetype = 'Text' file_name = ' ' file_content = 'ABCD' ) ).
    TRY.
        file_save_obj->create_files( files = files ).
      CATCH zcm_fio_checks INTO error.

        out->write( error->get_text(  ) ).

    ENDTRY.


    sample_data = VALUE #(
    (  book_name = 'The Psychology of Money'  author = 'Morgan Housel'  rating = '4.3' )
    (  book_name = 'The Millionaire Next Door'  author = 'Thomas J. Stanley, William D. Danko'  rating = '4.04' )
    (  book_name = 'Thinking, Fast and Slow'  author = 'Daniel Kahneman'  rating = '4.17' )
    ).

    DATA(csv_string) = convert_to_csv( sample_data  ).

    DATA(books_xtring) = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( csv_string ).
    files = VALUE #( ( file_mimetype = 'text/json' file_name = 'books.csv' file_content = books_xtring ) ).
    TRY.
        file_save_obj->create_files( files = files ).
        out->write( 'Records Updated' ).
      CATCH zcm_fio_checks INTO error.

        out->write( error->get_text(  ) ).

    ENDTRY.

*    DELETE FROM zfio_files WHERE file_uuid = '0E64B2B698161FD081ABA6AFAD20084B'.


  ENDMETHOD.
  METHOD convert_to_csv.

    TYPES: BEGIN OF ty_field_info,
             fieldname TYPE string,
           END OF ty_field_info.

    TYPES: ty_field_info_tab TYPE STANDARD TABLE OF ty_field_info WITH EMPTY KEY.

    DATA: ls_data       TYPE REF TO data,
          lt_fieldcat   TYPE ty_field_info_tab,
          ls_fieldcat   TYPE ty_field_info,
          lv_field_name TYPE string,
          lv_value      TYPE string,
          lv_csv_header TYPE string,
          lv_csv_row    TYPE string,
          lv_comma      TYPE string VALUE ','.

    DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr,
          lt_components   TYPE cl_abap_structdescr=>component_table,
          ls_component    TYPE cl_abap_structdescr=>component,
          lo_data_ref     TYPE REF TO data,
          lv_len          TYPE i.

    FIELD-SYMBOLS: <fs_data>  TYPE any,
                   <fs_field> TYPE any.

    IF it_data IS INITIAL.
      rv_csv = ''.
      RETURN.
    ENDIF.

    CREATE DATA lo_data_ref LIKE LINE OF it_data.
    ASSIGN lo_data_ref->* TO <fs_data>.

    lo_struct_descr ?= cl_abap_typedescr=>describe_by_data_ref( lo_data_ref ).
    lt_components = lo_struct_descr->get_components( ).

    LOOP AT lt_components INTO ls_component.
      ls_fieldcat-fieldname = ls_component-name.
      APPEND ls_fieldcat TO lt_fieldcat.
    ENDLOOP.

    LOOP AT lt_fieldcat INTO ls_fieldcat.
      lv_field_name = ls_fieldcat-fieldname.
      IF lv_csv_header IS NOT INITIAL.
        lv_csv_header = lv_csv_header && lv_comma.
      ENDIF.
      lv_csv_header = lv_csv_header && lv_field_name.
    ENDLOOP.

    rv_csv = lv_csv_header && cl_abap_char_utilities=>cr_lf.

    LOOP AT it_data ASSIGNING <fs_data>.
      CLEAR lv_csv_row.
      LOOP AT lt_fieldcat INTO ls_fieldcat.
        lv_field_name = ls_fieldcat-fieldname.
        ASSIGN COMPONENT lv_field_name OF STRUCTURE <fs_data> TO <fs_field>.
        lv_value = <fs_field>.

        " Escape commas and double quotes
        IF lv_value CS ',' OR lv_value CS '"'.
          REPLACE ALL OCCURRENCES OF '"' IN lv_value WITH '""'.
          CONCATENATE '"' lv_value '"' INTO lv_value.
        ENDIF.

        IF lv_csv_row IS NOT INITIAL.
          lv_csv_row = lv_csv_row && lv_comma.
        ENDIF.
        lv_csv_row = lv_csv_row && lv_value.
      ENDLOOP.
      rv_csv = rv_csv && lv_csv_row && cl_abap_char_utilities=>cr_lf.
    ENDLOOP.

    CONDENSE rv_csv NO-GAPS. "Remove trailing spaces and newlines

*    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf AT END OF rv_csv WITH ''.

*    lv_len = strlen( rv_csv ).
*    IF lv_len >= 2 AND rv_csv+lv_len-2(2) = cl_abap_char_utilities=>cr_lf.
*      rv_csv = rv_csv(lv_len - 2).
*    ENDIF.

  ENDMETHOD.

ENDCLASS.
