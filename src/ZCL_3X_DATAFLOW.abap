CLASS zcl_3x_dataflow DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF _ty_iobj_details,
        no       TYPE i,
        iobjnm   TYPE rsiobjnm,
        fieldnm  TYPE rsdiobjfieldnm,
        iobjtp   TYPE rsiobjtp,
        txtsh    TYPE rstxtsh,
        txtlg    TYPE rstxtlg,
        datatp   TYPE datatype_d,
        length   TYPE outputlen,
        decimals TYPE decimals,
        md_attr  TYPE rsattribfl, "Attribute zum IOBJ
        md_text  TYPE rsdtxttabfl, "Texte zum IOBJ
        md_hier  TYPE rshietabfl, "Hierarchien zum IOBJ
      END OF _ty_iobj_details .
    TYPES:
      tt_iobj_details TYPE STANDARD TABLE OF _ty_iobj_details WITH EMPTY KEY .
    TYPES:
      BEGIN OF _ty_src_tgt_map_3x,
        no                TYPE i,
        src_field         TYPE rstsfield-fieldnm,
        trans_field       TYPE rstsfield-iobjnm,
        transfer_rule(20) TYPE c,
        is_field          TYPE rsisfield-iobjnm,
        update_rule(20)   TYPE c,
        tgt_field         TYPE rsupdkey-iciobjnm,
        tgt_iobjtp        TYPE rsiobjtp,
        aggregation(13)   TYPE c,
        tgt_txtlg         TYPE rstxtlg,
      END OF _ty_src_tgt_map_3x .
    TYPES:
      tt_src_tgt_map_3x TYPE STANDARD TABLE OF _ty_src_tgt_map_3x WITH EMPTY KEY .

    CLASS-METHODS display_source_target_map_3x
      IMPORTING
        !i_datasource      TYPE roosourcer OPTIONAL
        !i_infosource      TYPE rsisfield-isource OPTIONAL
        !i_source_provider TYPE sobj_name OPTIONAL
        !i_target_provider TYPE sobj_name OPTIONAL
        !i_execution_mode  TYPE c
      RETURNING
        VALUE(r_table)     TYPE tt_src_tgt_map_3x .
    CLASS-METHODS check_object_type
      IMPORTING
        !i_object_name       TYPE string
      RETURNING
        VALUE(r_object_type) TYPE string .
    CLASS-METHODS get_all_fields_odso
      IMPORTING
        !i_odso        TYPE rsdodsobject
      RETURNING
        VALUE(r_table) TYPE tt_iobj_details .
    CLASS-METHODS get_all_fields_cube
      IMPORTING
        !i_cube        TYPE rsinfocube
      RETURNING
        VALUE(r_table) TYPE tt_iobj_details .
    CLASS-METHODS get_all_fields_iobj
      IMPORTING
        !i_iobj        TYPE rsinfocube
      RETURNING
        VALUE(r_table) TYPE tt_iobj_details .
protected section.
private section.

  class-methods MAP_DATASOURCE_PROVIDER_3X
    importing
      !I_T_TARGET_FIELDS type TT_SRC_TGT_MAP_3X
      !I_DATASOURCE type ROOSOURCER
      !I_INFOSOURCE type RSISFIELD-ISOURCE
      !I_TARGET_PROVIDER type SOBJ_NAME
    returning
      value(R_TABLE) type TT_SRC_TGT_MAP_3X .
  class-methods MAP_DATASOURCE_IOBJ_3X
    importing
      !I_T_TARGET_FIELDS type TT_SRC_TGT_MAP_3X
      !I_DATASOURCE type ROOSOURCER
      !I_TARGET_PROVIDER type SOBJ_NAME
    returning
      value(R_TABLE) type TT_SRC_TGT_MAP_3X .
  class-methods MAP_PROVIDER_PROVIDER_3X
    importing
      !I_T_TARGET_FIELDS type TT_SRC_TGT_MAP_3X
      !I_SOURCE_PROVIDER type SOBJ_NAME
      !I_TARGET_PROVIDER type SOBJ_NAME
    returning
      value(R_TABLE) type TT_SRC_TGT_MAP_3X .
ENDCLASS.



CLASS ZCL_3X_DATAFLOW IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_3X_DATAFLOW=>CHECK_OBJECT_TYPE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_OBJECT_NAME                  TYPE        STRING
* | [<-()] R_OBJECT_TYPE                  TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CHECK_OBJECT_TYPE.

    SELECT 'ADSO' AS type
    FROM rsoadso
    WHERE objvers = 'A'
    AND adsonm = @i_object_name
    INTO @r_object_type.
    ENDSELECT.

    SELECT 'ODSO' AS type
    FROM rsdodso
    WHERE objvers = 'A'
    AND odsobject = @i_object_name
    INTO @r_object_type.
    ENDSELECT.

    SELECT 'IOBJ' AS type
    FROM rsdiobj
    WHERE objvers = 'A'
    AND iobjnm = @i_object_name
    INTO @r_object_type.
    ENDSELECT.

    SELECT 'CUBE' AS type
    FROM rsdcube
    WHERE objvers = 'A'
    AND infocube = @i_object_name
    INTO @r_object_type.
    ENDSELECT.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_3X_DATAFLOW=>DISPLAY_SOURCE_TARGET_MAP_3X
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DATASOURCE                   TYPE        ROOSOURCER(optional)
* | [--->] I_INFOSOURCE                   TYPE        RSISFIELD-ISOURCE(optional)
* | [--->] I_SOURCE_PROVIDER              TYPE        SOBJ_NAME(optional)
* | [--->] I_TARGET_PROVIDER              TYPE        SOBJ_NAME(optional)
* | [--->] I_EXECUTION_MODE               TYPE        C
* | [<-()] R_TABLE                        TYPE        TT_SRC_TGT_MAP_3X
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD display_source_target_map_3x.

    DATA lt_target_fields TYPE STANDARD TABLE OF _ty_iobj_details WITH EMPTY KEY.
    DATA lt_src_tgt_map_3x TYPE STANDARD TABLE OF _ty_src_tgt_map_3x WITH EMPTY KEY.
    DATA lv_no TYPE i VALUE 1.
    TYPES: BEGIN OF _ty_transfer_rule,
             is_field        TYPE rsiobjnm,
             tgt_field       TYPE rsiobjnm,
             feldregel(20)   TYPE c,
             aggregation(13) TYPE c,
           END OF _ty_transfer_rule.
    DATA lt_transferregeln TYPE STANDARD TABLE OF _ty_transfer_rule WITH EMPTY KEY.
    TYPES: BEGIN OF _ty_ueb,
             feldregel(13) TYPE c,
             src_field     TYPE rsiobjnm_ks,
             is_field      TYPE rsiobjnm_ks,
           END OF _ty_ueb.
    DATA lt_ueb TYPE STANDARD TABLE OF _ty_ueb WITH EMPTY KEY.

    DATA(lv_target_type) = zcl_3x_dataflow=>check_object_type( i_object_name = CONV #( i_target_provider ) ).

    IF lv_target_type = 'CUBE'.
      lt_target_fields = zcl_3x_dataflow=>get_all_fields_cube( CONV #( i_target_provider ) ).
    ELSEIF lv_target_type = 'ODSO'.
      lt_target_fields = zcl_3x_dataflow=>get_all_fields_odso( CONV #( i_target_provider ) ).
    ELSEIF lv_target_type = 'IOBJ'.
      lt_target_fields = zcl_3x_dataflow=>get_all_fields_iobj( CONV #( i_target_provider ) ).
    ENDIF.

    LOOP AT lt_target_fields ASSIGNING FIELD-SYMBOL(<fs_target_fields>).
      INSERT VALUE #( no = lv_no tgt_field = <fs_target_fields>-iobjnm tgt_iobjtp = <fs_target_fields>-iobjtp tgt_txtlg = <fs_target_fields>-txtlg )
        INTO TABLE lt_src_tgt_map_3x.
      lv_no = lv_no + 1.
    ENDLOOP.

    IF i_execution_mode = '0'. "DataSource auf InfoProvider
      r_table = zcl_3x_dataflow=>map_datasource_provider_3x( i_t_target_fields = lt_src_tgt_map_3x
                                                          i_datasource = i_datasource
                                                          i_infosource = i_infosource
                                                          i_target_provider = i_target_provider ).
    ELSEIF i_execution_mode = '1'. "InfoProvider auf InfoProvider
      r_table = zcl_3x_dataflow=>map_provider_provider_3x( i_t_target_fields = lt_src_tgt_map_3x
                                                        i_source_provider = i_source_provider
                                                        i_target_provider = i_target_provider ).
    ELSEIF i_execution_mode = '2'. "DataSource auf InfoObject (Stammdaten Attribute)
      r_table = zcl_3x_dataflow=>map_datasource_iobj_3x( i_t_target_fields = lt_src_tgt_map_3x
                                                  i_datasource = i_datasource
                                                  i_target_provider = i_target_provider ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_3X_DATAFLOW=>GET_ALL_FIELDS_CUBE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CUBE                         TYPE        RSINFOCUBE
* | [<-()] R_TABLE                        TYPE        TT_IOBJ_DETAILS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_ALL_FIELDS_CUBE.

    DATA LT_CUBE TYPE STANDARD TABLE OF _TY_IOBJ_DETAILS WITH EMPTY KEY.
    DATA O_IOBJ_DETAILS TYPE REF TO CL_RSD_IOBJ.
    DATA LS_IOBJ_DETAILS TYPE RSD_S_VIOBJ.
    DATA LV_NO TYPE I VALUE 1.

    SELECT IOBJNM
     FROM RSDCUBEIOBJ
     WHERE INFOCUBE = @I_CUBE
     AND OBJVERS = 'A'
     INTO CORRESPONDING FIELDS OF TABLE @LT_CUBE.
    DELETE LT_CUBE WHERE IOBJNM CS '__'.

    LOOP AT LT_CUBE ASSIGNING FIELD-SYMBOL(<FS_CUBE>).
      O_IOBJ_DETAILS = CL_RSD_IOBJ=>FACTORY( <FS_CUBE>-IOBJNM ).
      O_IOBJ_DETAILS->GET_INFO( EXPORTING I_OBJVERS = 'A' IMPORTING E_S_VIOBJ = LS_IOBJ_DETAILS ).
      <FS_CUBE>-NO = LV_NO.
      <FS_CUBE>-FIELDNM = LS_IOBJ_DETAILS-FIELDNM.
      <FS_CUBE>-IOBJTP = LS_IOBJ_DETAILS-IOBJTP.
      <FS_CUBE>-TXTSH = LS_IOBJ_DETAILS-TXTSH.
      <FS_CUBE>-TXTLG = LS_IOBJ_DETAILS-TXTLG.
      <FS_CUBE>-DATATP = LS_IOBJ_DETAILS-DATATP.
      <FS_CUBE>-LENGTH = LS_IOBJ_DETAILS-OUTPUTLEN.
      <FS_CUBE>-DECIMALS = LS_IOBJ_DETAILS-KYFDECIM.
      <FS_CUBE>-MD_ATTR = LS_IOBJ_DETAILS-ATTRIBFL.
      <FS_CUBE>-MD_TEXT  = SWITCH #( LS_IOBJ_DETAILS-TXTTABFL WHEN '1' THEN 'X'
                                                                 WHEN '0' THEN '' ).
      <FS_CUBE>-MD_HIER = LS_IOBJ_DETAILS-HIETABFL.
      LV_NO = LV_NO + 1.
    ENDLOOP.

    R_TABLE = LT_CUBE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_3X_DATAFLOW=>GET_ALL_FIELDS_IOBJ
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_IOBJ                         TYPE        RSINFOCUBE
* | [<-()] R_TABLE                        TYPE        TT_IOBJ_DETAILS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_all_fields_iobj.

    DATA lt_iobj TYPE STANDARD TABLE OF _ty_iobj_details WITH EMPTY KEY.
    DATA o_iobj_details TYPE REF TO cl_rsd_iobj.
    DATA ls_iobj_details TYPE rsd_s_viobj.
    DATA lv_no TYPE i VALUE 1.

    SELECT attrinm AS iobjnm
     FROM rsdbchatr
     WHERE chabasnm = @i_iobj
     AND objvers = 'A'
     INTO CORRESPONDING FIELDS OF TABLE @lt_iobj.

    INSERT VALUE #( iobjnm = i_iobj ) INTO TABLE lt_iobj.

    LOOP AT lt_iobj ASSIGNING FIELD-SYMBOL(<fs_iobj>).
      o_iobj_details = cl_rsd_iobj=>factory( <fs_iobj>-iobjnm ).
      o_iobj_details->get_info( EXPORTING i_objvers = 'A' IMPORTING e_s_viobj = ls_iobj_details ).
      <fs_iobj>-no = lv_no.
      <fs_iobj>-fieldnm = ls_iobj_details-fieldnm.
      <fs_iobj>-iobjtp = ls_iobj_details-iobjtp.
      <fs_iobj>-txtsh = ls_iobj_details-txtsh.
      <fs_iobj>-txtlg = ls_iobj_details-txtlg.
      <fs_iobj>-datatp = ls_iobj_details-datatp.
      <fs_iobj>-length = ls_iobj_details-outputlen.
      <fs_iobj>-decimals = ls_iobj_details-kyfdecim.
      <fs_iobj>-md_attr = ls_iobj_details-attribfl.
      <fs_iobj>-md_text  = SWITCH #( ls_iobj_details-txttabfl WHEN '1' THEN 'X'
                                                                 WHEN '0' THEN '' ).
      <fs_iobj>-md_hier = ls_iobj_details-hietabfl.
      lv_no = lv_no + 1.
    ENDLOOP.

    r_table = lt_iobj.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_3X_DATAFLOW=>GET_ALL_FIELDS_ODSO
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ODSO                         TYPE        RSDODSOBJECT
* | [<-()] R_TABLE                        TYPE        TT_IOBJ_DETAILS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_ALL_FIELDS_ODSO.

    DATA LT_ODSO TYPE STANDARD TABLE OF _TY_IOBJ_DETAILS WITH EMPTY KEY.
    DATA O_IOBJ_DETAILS TYPE REF TO CL_RSD_IOBJ.
    DATA LS_IOBJ_DETAILS TYPE RSD_S_VIOBJ.
    DATA lv_no type i value 1.

    SELECT IOBJNM
     FROM RSDODSOIOBJ
     WHERE ODSOBJECT = @I_ODSO
     AND OBJVERS = 'A'
     INTO CORRESPONDING FIELDS OF TABLE @LT_ODSO.

    LOOP AT LT_ODSO ASSIGNING FIELD-SYMBOL(<FS_ODSO>).
      O_IOBJ_DETAILS = CL_RSD_IOBJ=>FACTORY( <FS_ODSO>-IOBJNM ).
      O_IOBJ_DETAILS->GET_INFO( EXPORTING I_OBJVERS = 'A' IMPORTING E_S_VIOBJ = LS_IOBJ_DETAILS ).
      <fs_odso>-no = lv_no.
      <FS_ODSO>-FIELDNM = LS_IOBJ_DETAILS-FIELDNM.
      <FS_ODSO>-IOBJTP = LS_IOBJ_DETAILS-IOBJTP.
      <FS_ODSO>-TXTSH = LS_IOBJ_DETAILS-TXTSH.
      <FS_ODSO>-TXTLG = LS_IOBJ_DETAILS-TXTLG.
      <FS_ODSO>-DATATP = LS_IOBJ_DETAILS-DATATP.
      <FS_ODSO>-LENGTH = LS_IOBJ_DETAILS-OUTPUTLEN.
      <FS_ODSO>-DECIMALS = LS_IOBJ_DETAILS-KYFDECIM.
      <FS_ODSO>-MD_ATTR = LS_IOBJ_DETAILS-ATTRIBFL.
      <FS_ODSO>-MD_TEXT  = SWITCH #( LS_IOBJ_DETAILS-TXTTABFL WHEN '1' THEN 'X'
                                                                 WHEN '0' THEN '' ).
      <FS_ODSO>-MD_HIER = LS_IOBJ_DETAILS-HIETABFL.
      lv_no = lv_no + 1.
    ENDLOOP.

    R_TABLE = LT_ODSO.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_3X_DATAFLOW=>MAP_DATASOURCE_IOBJ_3X
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_T_TARGET_FIELDS              TYPE        TT_SRC_TGT_MAP_3X
* | [--->] I_DATASOURCE                   TYPE        ROOSOURCER
* | [--->] I_TARGET_PROVIDER              TYPE        SOBJ_NAME
* | [<-()] R_TABLE                        TYPE        TT_SRC_TGT_MAP_3X
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD MAP_DATASOURCE_IOBJ_3X.

    "-- Ergebnis der Methode:
    "-- Gib das Mapping zwischen DataSource und InfoObject (Stammdaten-Attribute) zurück.


    TYPES: BEGIN OF _ty_ueb,
             src_field     TYPE rstsfield-fieldnm,
             trans_field   TYPE rstsfield-iobjnm,
             feldregel(13) TYPE c,
             is_field      TYPE rsiobjnm_ks,
           END OF _ty_ueb.
    DATA lt_ueb TYPE STANDARD TABLE OF _ty_ueb WITH EMPTY KEY.
    DATA lt_target_fields TYPE STANDARD TABLE OF _ty_iobj_details WITH EMPTY KEY.
    DATA lt_src_tgt_map_3x TYPE STANDARD TABLE OF _ty_src_tgt_map_3x.
    DATA lv_no TYPE i VALUE 1.
    DATA lv_where_string TYPE string.

    lt_src_tgt_map_3x = i_t_target_fields.


    "-- Selektiere eindeutige Übertragungsregel ID (transfer rule)
    SELECT DISTINCT transtru
      FROM rsisosmap
      WHERE objvers = 'A'
      AND oltpsource = @i_datasource
      AND isource = @i_target_provider
      INTO @DATA(lv_transtru).
      ENDSELECT.


    "-- MAPPING: untere Trfn (Felder DataSource zu Transferstruktur zu InfoSource/Kommunikationsstruktur)
    SELECT  src2trans~fieldnm   AS src_field,
            trans2is~iobjnm     AS is_field,
            trans2is~iobjnm_ts  AS trans_field,
            CASE  WHEN trans2is~fixed_value <> ' ' THEN 'Konstante'
                  WHEN trans2is~convrout_g <> ' ' THEN 'Routine'
                  WHEN trans2is~convrout_l <> ' ' THEN 'Routine'
                  WHEN trans2is~formula_id <> ' ' THEN 'Formel'
                  ELSE '1:1'
            END                 AS feldregel
      FROM rstsrules AS trans2is
            LEFT OUTER JOIN rstsfield AS src2trans
            ON trans2is~transtru = src2trans~transtru
            AND src2trans~objvers = 'A'
            AND trans2is~objvers = 'A'
      WHERE trans2is~transtru = @lv_transtru
      AND src2trans~transtru = @lv_transtru
      AND src2trans~iobjnm = trans2is~iobjnm_ts
UNION
    SELECT  ' '                 AS src_field,
            trans2is~iobjnm     AS is_field,
            trans2is~iobjnm_ts  AS trans_field,
            CASE  WHEN trans2is~fixed_value <> ' ' THEN 'Konstante'
                  WHEN trans2is~convrout_g <> ' ' THEN 'Routine'
                  WHEN trans2is~convrout_l <> ' ' THEN 'Routine'
                  WHEN trans2is~formula_id <> ' ' THEN 'Formel'
                  ELSE ' '
            END                 AS feldregel
      FROM rstsrules AS trans2is
      WHERE trans2is~transtru = @lv_transtru
      AND trans2is~objvers = 'A'
      AND iobjnm_ts = ''
      INTO CORRESPONDING FIELDS OF TABLE @lt_ueb. "Felder Transferstruktur zu Kommunikationsstruktur/InfoSource (untere Trfn)



    LOOP AT lt_src_tgt_map_3x ASSIGNING FIELD-SYMBOL(<fs_src_tgt>).
      <fs_src_tgt>-transfer_rule = VALUE #( lt_ueb[ is_field = <fs_src_tgt>-tgt_field ]-feldregel OPTIONAL ).
      <fs_src_tgt>-src_field = VALUE #( lt_ueb[ is_field = <fs_src_tgt>-tgt_field ]-src_field OPTIONAL ).
    ENDLOOP.

    r_table = lt_src_tgt_map_3x.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_3X_DATAFLOW=>MAP_DATASOURCE_PROVIDER_3X
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_T_TARGET_FIELDS              TYPE        TT_SRC_TGT_MAP_3X
* | [--->] I_DATASOURCE                   TYPE        ROOSOURCER
* | [--->] I_INFOSOURCE                   TYPE        RSISFIELD-ISOURCE
* | [--->] I_TARGET_PROVIDER              TYPE        SOBJ_NAME
* | [<-()] R_TABLE                        TYPE        TT_SRC_TGT_MAP_3X
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD map_datasource_provider_3x.

    "-- Ergebnis der Methode:
    "-- Gib das Mapping zwischen DataSource und InfoProvider zurück.
    "-- DataSource -> Übertragungsregeln (transfer rules) -> InfoSource -> Fortschreibungsregeln (update rules) -> InfoProvider

    TYPES: BEGIN OF _ty_transfer_rule,
             is_field        TYPE rsiobjnm,
             tgt_field       TYPE rsiobjnm,
             feldregel(20)   TYPE c,
             aggregation(13) TYPE c,
           END OF _ty_transfer_rule.
    DATA lt_transferregeln TYPE STANDARD TABLE OF _ty_transfer_rule WITH EMPTY KEY.
    TYPES: BEGIN OF _ty_ueb,
             src_field     TYPE rstsfield-fieldnm,
             trans_field   TYPE rstsfield-iobjnm,
             feldregel(13) TYPE c,
             is_field      TYPE rsiobjnm_ks,
           END OF _ty_ueb.
    DATA lt_ueb TYPE STANDARD TABLE OF _ty_ueb WITH EMPTY KEY.
    DATA lt_target_fields TYPE STANDARD TABLE OF _ty_iobj_details WITH EMPTY KEY.
    DATA lt_src_tgt_map_3x TYPE STANDARD TABLE OF _ty_src_tgt_map_3x.
    DATA lv_no TYPE i VALUE 1.
    DATA lv_where_string TYPE string.
    DATA lv_updid TYPE rsupdid.
    DATA lv_startroutine TYPE rsroutine.

    lt_src_tgt_map_3x = i_t_target_fields.



    SELECT DISTINCT updid, startroutine
      FROM rsupdinfo
      WHERE objvers = 'A'
      AND isource = @i_infosource
      AND infocube = @i_target_provider
      INTO (@lv_updid, @lv_startroutine). "eindeutige ID für Fortschreibungsregeln (update rule)
    ENDSELECT.

    SELECT DISTINCT transtru
      FROM rsisosmap
      WHERE objvers = 'A'
      AND oltpsource = @i_datasource
      AND isource = @i_infosource
      INTO @DATA(lv_transtru). "eindeutige Übertragungsregel ID (transfer rule)
    ENDSELECT.



    "-- MAPPING 1: untere Trfn (Felder DataSource zu Transferstruktur zu InfoSource/Kommunikationsstruktur)
    SELECT  src2trans~fieldnm   AS src_field,
            trans2is~iobjnm     AS is_field,
            trans2is~iobjnm_ts  AS trans_field,
            CASE  WHEN trans2is~fixed_value <> ' ' THEN 'Konstante'
                  WHEN trans2is~convrout_g <> ' ' THEN 'Routine'
                  WHEN trans2is~convrout_l <> ' ' THEN 'Routine'
                  WHEN trans2is~formula_id <> ' ' THEN 'Formel'
                  ELSE '1:1'
            END                 AS feldregel
      FROM rstsrules AS trans2is
            LEFT OUTER JOIN rstsfield AS src2trans
            ON trans2is~transtru = src2trans~transtru
            AND src2trans~objvers = 'A'
            AND trans2is~objvers = 'A'
      WHERE trans2is~transtru = @lv_transtru
      AND src2trans~transtru = @lv_transtru
      AND src2trans~iobjnm = trans2is~iobjnm_ts
UNION
    SELECT  ' '                 AS src_field,
            trans2is~iobjnm     AS is_field,
            trans2is~iobjnm_ts  AS trans_field,
            CASE  WHEN trans2is~fixed_value <> ' ' THEN 'Konstante'
                  WHEN trans2is~convrout_g <> ' ' THEN 'Routine'
                  WHEN trans2is~convrout_l <> ' ' THEN 'Routine'
                  WHEN trans2is~formula_id <> ' ' THEN 'Formel'
                  ELSE ' '
            END                 AS feldregel
      FROM rstsrules AS trans2is
      WHERE trans2is~transtru = @lv_transtru
      AND trans2is~objvers = 'A'
      AND iobjnm_ts = ''
      INTO CORRESPONDING FIELDS OF TABLE @lt_ueb. "Felder Transferstruktur zu Kommunikationsstruktur/InfoSource (untere Trfn)



    "-- MAPPING 2: obere Trfn (InfoSource zu Target InfoProvider)
    SELECT  iciobjnm  AS tgt_field,
            csiobjnm  AS is_field,
            ' ' AS aggregation,
            CASE calctpkey  WHEN '0' THEN ' '
                            WHEN '1' THEN '1:1'
                            WHEN '2' THEN 'Formel'
                            WHEN '3' THEN 'Routine'
                            WHEN '4' THEN 'Konstante'
                            WHEN '5' THEN 'Stammdaten nachlesen'
                            ELSE 'N.V.'
             END      AS feldregel
      FROM rsupdkey
      WHERE objvers = 'A'
      AND updid = @lv_updid
      AND csiobjnm <> ''
 UNION
    SELECT  iciobjnm AS tgt_field,
            csiobjnm AS is_field,
            CASE updtype  WHEN 'MOV' THEN 'Überschreiben'
                          WHEN 'ADD' THEN 'Summieren'
                          ELSE 'No updating'
            END      AS aggregation,
            CASE calctpdat  WHEN '1' THEN '1:1'
                            WHEN '2' THEN 'Formel'
                            WHEN '3' THEN 'Routine'
                            ELSE 'N.V.'
            END      AS feldregel
      FROM rsupddat
      WHERE objvers = 'A'
      AND updid = @lv_updid
      INTO CORRESPONDING FIELDS OF TABLE @lt_transferregeln.



    "-- VORBEREITUNG Ausgabe
    LOOP AT lt_transferregeln ASSIGNING FIELD-SYMBOL(<fs_trfnr>). "entferne für Merkmale den Aggregations-Wert; behalte ihn nur für Kennzahlen
      IF line_exists( lt_src_tgt_map_3x[ tgt_field = <fs_trfnr>-tgt_field tgt_iobjtp = 'KYF' ] ).
      ELSE.
        <fs_trfnr>-aggregation = ''.
      ENDIF.
    ENDLOOP.



    "-- EINTRÄGE IN AUSGABETABELLE einfügen
    LOOP AT lt_src_tgt_map_3x ASSIGNING FIELD-SYMBOL(<fs_src_tgt>).
      <fs_src_tgt>-aggregation = VALUE #( lt_transferregeln[ tgt_field = <fs_src_tgt>-tgt_field ]-aggregation OPTIONAL ).
      <fs_src_tgt>-update_rule = VALUE #( lt_transferregeln[ tgt_field = <fs_src_tgt>-tgt_field ]-feldregel OPTIONAL ).
      <fs_src_tgt>-is_field = VALUE #( lt_transferregeln[ tgt_field = <fs_src_tgt>-tgt_field ]-is_field OPTIONAL ).
      <fs_src_tgt>-transfer_rule = VALUE #( lt_ueb[ is_field = <fs_src_tgt>-is_field ]-feldregel OPTIONAL ).
      <fs_src_tgt>-src_field = VALUE #( lt_ueb[ is_field = <fs_src_tgt>-is_field ]-src_field OPTIONAL ).
      <fs_src_tgt>-trans_field = VALUE #( lt_ueb[ is_field = <fs_src_tgt>-is_field ]-trans_field OPTIONAL ).
    ENDLOOP.

    LOOP AT lt_src_tgt_map_3x ASSIGNING <fs_src_tgt> WHERE ( update_rule = ' ' OR update_rule = '' OR update_rule = 'Routine' ).
      <fs_src_tgt>-trans_field = ''.
    ENDLOOP.

    IF lv_startroutine = '0000'. "wenn Startroutine in Fortschreibungsregeln existiert, Zeile in Ausgabe bringen
    ELSE.
      INSERT VALUE #( update_rule = 'STARTROUTINE' ) INTO TABLE lt_src_tgt_map_3x.
    ENDIF.

    r_table = lt_src_tgt_map_3x.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_3X_DATAFLOW=>MAP_PROVIDER_PROVIDER_3X
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_T_TARGET_FIELDS              TYPE        TT_SRC_TGT_MAP_3X
* | [--->] I_SOURCE_PROVIDER              TYPE        SOBJ_NAME
* | [--->] I_TARGET_PROVIDER              TYPE        SOBJ_NAME
* | [<-()] R_TABLE                        TYPE        TT_SRC_TGT_MAP_3X
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD MAP_PROVIDER_PROVIDER_3X.

    "-- Ergebnis der Methode:
    "-- Gib das Mapping zwischen InfoProvider und InfoProvider zurück.
    "-- Diese Datenflüsse haben keine Übertragungsregeln und die Quelle ist keine DataSource, sondern ein InfoProvider.
    "-- Quell-InfoProvider -> Fortschreibungsregeln (update rules) -> Ziel-InfoProvider

    TYPES: BEGIN OF _ty_transfer_rule,
             src_field       TYPE rsiobjnm,
             tgt_field       TYPE rsiobjnm,
             feldregel(20)   TYPE c,
             aggregation(13) TYPE c,
           END OF _ty_transfer_rule.
    DATA lt_transferregeln TYPE STANDARD TABLE OF _ty_transfer_rule WITH EMPTY KEY.
    DATA lt_target_fields TYPE STANDARD TABLE OF _ty_iobj_details WITH EMPTY KEY.
    DATA lt_src_tgt_map_3x TYPE STANDARD TABLE OF _ty_src_tgt_map_3x.
    DATA lv_no TYPE i VALUE 1.
    DATA lv_where_string TYPE string.

    lt_src_tgt_map_3x = i_t_target_fields.

    "-- Selektiere eindeutige Fortschreibungsregel ID
    lv_where_string = |isource LIKE '%{ i_source_provider }'|.

    SELECT DISTINCT updid
      FROM rsupdinfo
      WHERE objvers = 'A'
      AND (lv_where_string)
      AND infocube = @i_target_provider
      INTO @DATA(lv_updid).
    ENDSELECT.


    "-- Mapping der "Trfn" | Quell-Provider zu Ziel-Provider
    SELECT iciobjnm AS tgt_field, csiobjnm AS src_field, ' ' AS aggregation,
         CASE calctpkey WHEN '0' THEN ' '
                        WHEN '1' THEN '1:1'
                        WHEN '2' THEN 'Formel'
                        WHEN '3' THEN 'Routine'
                        WHEN '4' THEN 'Konstante'
                        WHEN '5' THEN 'Stammdaten nachlesen'
                        ELSE 'N.V.'   END AS feldregel
      FROM rsupdkey
      WHERE objvers = 'A'
      AND updid = @lv_updid
    UNION
    SELECT iciobjnm AS tgt_field, csiobjnm AS src_field,
        CASE updtype  WHEN 'MOV' THEN 'Überschreiben'
                      WHEN 'ADD' THEN 'Summieren'
                      ELSE 'No updating' END AS aggregation,
        CASE calctpdat  WHEN '1' THEN '1:1'
                        WHEN '2' THEN 'Formel'
                        WHEN '3' THEN 'Routine'
                        ELSE 'N.V.'   END AS feldregel
      FROM rsupddat
      WHERE objvers = 'A'
      AND updid = @lv_updid
      INTO CORRESPONDING FIELDS OF TABLE @lt_transferregeln.


    "-- Entferne für Merkmale den Aggregations-Wert; behalte ihn nur für Kennzahlen
    LOOP AT lt_transferregeln ASSIGNING FIELD-SYMBOL(<fs_trfnr>).
      IF line_exists( lt_src_tgt_map_3x[ tgt_field = <fs_trfnr>-tgt_field tgt_iobjtp = 'KYF' ] ).
        "nur Aggregationstyp von Kennzahlen sind richtig
      ELSE.
        <fs_trfnr>-aggregation = ''.
      ENDIF.
    ENDLOOP.


    LOOP AT lt_src_tgt_map_3x ASSIGNING FIELD-SYMBOL(<fs_src_tgt>).
      <fs_src_tgt>-aggregation = VALUE #( lt_transferregeln[ tgt_field = <fs_src_tgt>-tgt_field ]-aggregation OPTIONAL ).
      <fs_src_tgt>-update_rule = VALUE #( lt_transferregeln[ tgt_field = <fs_src_tgt>-tgt_field ]-feldregel OPTIONAL ).
      <fs_src_tgt>-src_field = VALUE #( lt_transferregeln[ tgt_field = <fs_src_tgt>-tgt_field ]-src_field OPTIONAL ).
    ENDLOOP.

    LOOP AT lt_src_tgt_map_3x ASSIGNING <fs_src_tgt> WHERE ( update_rule = ' ' OR update_rule = '' OR update_rule = 'Routine' ).
      <fs_src_tgt>-src_field = ''.
    ENDLOOP.


    r_table = lt_src_tgt_map_3x.

  ENDMETHOD.
ENDCLASS.
